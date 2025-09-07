;; Claim Processor Smart Contract
;; Handles claim submission, validation, and payout processing for decentralized insurance pools

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-INVALID-CLAIM (err u101))
(define-constant ERR-CLAIM-NOT-FOUND (err u102))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u103))
(define-constant ERR-INSUFFICIENT-POOL-BALANCE (err u104))
(define-constant ERR-INVALID-AMOUNT (err u105))
(define-constant ERR-CLAIM-EXPIRED (err u106))
(define-constant ERR-UNAUTHORIZED (err u107))
(define-constant ERR-INVALID-STATUS (err u108))

;; Data Variables
(define-data-var next-claim-id uint u1)
(define-data-var total-claims-submitted uint u0)
(define-data-var total-claims-processed uint u0)
(define-data-var total-payouts-made uint u0)
(define-data-var contract-active bool true)

;; Data Maps
(define-map claims 
    uint 
    {
        claimant: principal,
        pool-id: uint,
        amount-requested: uint,
        amount-approved: uint,
        description: (string-ascii 500),
        status: (string-ascii 20),
        submission-time: uint,
        processing-time: (optional uint),
        processor: (optional principal),
        evidence-hash: (optional (buff 32))
    }
)

(define-map pool-balances uint uint)
(define-map pool-managers uint principal)
(define-map claim-validators principal bool)
(define-map user-claim-history principal (list 50 uint))
(define-map claim-processing-history uint (list 10 {processor: principal, action: (string-ascii 50), timestamp: uint}))

;; Private Functions

;; Validate claim data
(define-private (is-valid-claim-data (pool-id uint) (amount uint) (description (string-ascii 500)))
    (and 
        (> pool-id u0)
        (> amount u0)
        (> (len description) u0)
        (<= amount u1000000000) ;; Maximum claim amount (10^9 microSTX)
    )
)

;; Check if caller is authorized pool manager
(define-private (is-pool-manager (pool-id uint) (caller principal))
    (is-eq (some caller) (map-get? pool-managers pool-id))
)

;; Check if caller is claim validator
(define-private (is-claim-validator (caller principal))
    (default-to false (map-get? claim-validators caller))
)

;; Get current block height as timestamp
(define-private (get-current-time)
    block-height
)

;; Update user claim history
(define-private (update-user-history (user principal) (claim-id uint))
    (let ((current-history (default-to (list) (map-get? user-claim-history user))))
        (if (< (len current-history) u50)
            (map-set user-claim-history user (unwrap-panic (as-max-len? (append current-history claim-id) u50)))
            (map-set user-claim-history user (unwrap-panic (as-max-len? (append (unwrap-panic (slice? current-history u1 u50)) claim-id) u50)))
        )
    )
)

;; Add processing history entry
(define-private (add-processing-entry (claim-id uint) (processor principal) (action (string-ascii 50)))
    (let ((current-history (default-to (list) (map-get? claim-processing-history claim-id))))
        (if (< (len current-history) u10)
            (map-set claim-processing-history claim-id 
                (unwrap-panic (as-max-len? 
                    (append current-history {processor: processor, action: action, timestamp: (get-current-time)}) 
                    u10)))
            (map-set claim-processing-history claim-id 
                (unwrap-panic (as-max-len? 
                    (append (unwrap-panic (slice? current-history u1 u10)) 
                        {processor: processor, action: action, timestamp: (get-current-time)}) 
                    u10)))
        )
    )
)

;; Public Functions

;; Submit a new claim
(define-public (submit-claim (pool-id uint) (amount uint) (description (string-ascii 500)) (evidence-hash (optional (buff 32))))
    (let 
        (
            (claim-id (var-get next-claim-id))
            (current-time (get-current-time))
        )
        (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
        (asserts! (is-valid-claim-data pool-id amount description) ERR-INVALID-CLAIM)
        
        (map-set claims claim-id {
            claimant: tx-sender,
            pool-id: pool-id,
            amount-requested: amount,
            amount-approved: u0,
            description: description,
            status: "pending",
            submission-time: current-time,
            processing-time: none,
            processor: none,
            evidence-hash: evidence-hash
        })
        
        (update-user-history tx-sender claim-id)
        (add-processing-entry claim-id tx-sender "submitted")
        (var-set next-claim-id (+ claim-id u1))
        (var-set total-claims-submitted (+ (var-get total-claims-submitted) u1))
        
        (ok claim-id)
    )
)

;; Process a claim (approve or reject)
(define-public (process-claim (claim-id uint) (approved-amount uint) (new-status (string-ascii 20)))
    (let ((claim (unwrap! (map-get? claims claim-id) ERR-CLAIM-NOT-FOUND)))
        (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
        (asserts! (or (is-claim-validator tx-sender) 
                     (is-pool-manager (get pool-id claim) tx-sender) 
                     (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status claim) "pending") ERR-CLAIM-ALREADY-PROCESSED)
        (asserts! (or (is-eq new-status "approved") (is-eq new-status "rejected")) ERR-INVALID-STATUS)
        
        (let ((final-approved-amount 
            (if (is-eq new-status "approved")
                (begin
                    (asserts! (> approved-amount u0) ERR-INVALID-AMOUNT)
                    (asserts! (<= approved-amount (get amount-requested claim)) ERR-INVALID-AMOUNT)
                    (let ((pool-balance (default-to u0 (map-get? pool-balances (get pool-id claim)))))
                        (asserts! (>= pool-balance approved-amount) ERR-INSUFFICIENT-POOL-BALANCE)
                        (map-set pool-balances (get pool-id claim) (- pool-balance approved-amount))
                        (var-set total-payouts-made (+ (var-get total-payouts-made) approved-amount))
                    )
                    approved-amount
                )
                u0
            )))
            
            (map-set claims claim-id (merge claim {
                amount-approved: final-approved-amount,
                status: new-status,
                processing-time: (some (get-current-time)),
                processor: (some tx-sender)
            }))
            
            (add-processing-entry claim-id tx-sender (if (is-eq new-status "approved") "approved" "rejected"))
            (var-set total-claims-processed (+ (var-get total-claims-processed) u1))
            
            (ok true)
        )
    )
)

;; Execute payout for approved claim
(define-public (execute-payout (claim-id uint))
    (let ((claim (unwrap! (map-get? claims claim-id) ERR-CLAIM-NOT-FOUND)))
        (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
        (asserts! (or (is-claim-validator tx-sender) 
                     (is-pool-manager (get pool-id claim) tx-sender) 
                     (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get status claim) "approved") ERR-INVALID-STATUS)
        (asserts! (> (get amount-approved claim) u0) ERR-INVALID-AMOUNT)
        
        ;; In a real implementation, this would transfer STX to the claimant
        ;; For now, we'll just mark the claim as paid
        (map-set claims claim-id (merge claim {status: "paid"}))
        (add-processing-entry claim-id tx-sender "payout-executed")
        
        (ok (get amount-approved claim))
    )
)

;; Administrative Functions

;; Set pool balance (only pool manager)
(define-public (set-pool-balance (pool-id uint) (balance uint))
    (begin
        (asserts! (is-pool-manager pool-id tx-sender) ERR-UNAUTHORIZED)
        (map-set pool-balances pool-id balance)
        (ok true)
    )
)

;; Add pool manager (only contract owner)
(define-public (add-pool-manager (pool-id uint) (manager principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (map-set pool-managers pool-id manager)
        (ok true)
    )
)

;; Add claim validator (only contract owner)
(define-public (add-claim-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (map-set claim-validators validator true)
        (ok true)
    )
)

;; Remove claim validator (only contract owner)
(define-public (remove-claim-validator (validator principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (map-delete claim-validators validator)
        (ok true)
    )
)

;; Toggle contract active status (only contract owner)
(define-public (toggle-contract-status)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (var-set contract-active (not (var-get contract-active)))
        (ok (var-get contract-active))
    )
)

;; Read-only Functions

;; Get claim details
(define-read-only (get-claim (claim-id uint))
    (map-get? claims claim-id)
)

;; Get pool balance
(define-read-only (get-pool-balance (pool-id uint))
    (default-to u0 (map-get? pool-balances pool-id))
)

;; Get user claim history
(define-read-only (get-user-claim-history (user principal))
    (default-to (list) (map-get? user-claim-history user))
)

;; Get claim processing history
(define-read-only (get-claim-processing-history (claim-id uint))
    (default-to (list) (map-get? claim-processing-history claim-id))
)

;; Get contract statistics
(define-read-only (get-contract-stats)
    {
        total-claims-submitted: (var-get total-claims-submitted),
        total-claims-processed: (var-get total-claims-processed),
        total-payouts-made: (var-get total-payouts-made),
        next-claim-id: (var-get next-claim-id),
        contract-active: (var-get contract-active)
    }
)

;; Check if user is claim validator
(define-read-only (is-validator (user principal))
    (default-to false (map-get? claim-validators user))
)

;; Get pool manager
(define-read-only (get-pool-manager (pool-id uint))
    (map-get? pool-managers pool-id)
)

;; Get contract owner
(define-read-only (get-contract-owner)
    CONTRACT-OWNER
)

;; Check claim status
(define-read-only (get-claim-status (claim-id uint))
    (match (map-get? claims claim-id)
        claim (some (get status claim))
        none
    )
)
