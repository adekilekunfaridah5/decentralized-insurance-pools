;; Insurance Pool Smart Contract
;; Manages insurance pools, premium collection, and policy management for decentralized insurance

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u200))
(define-constant ERR-INVALID-POOL (err u201))
(define-constant ERR-POOL-NOT-FOUND (err u202))
(define-constant ERR-POOL-CLOSED (err u203))
(define-constant ERR-INSUFFICIENT-PREMIUM (err u204))
(define-constant ERR-INVALID-AMOUNT (err u205))
(define-constant ERR-ALREADY-MEMBER (err u206))
(define-constant ERR-NOT-MEMBER (err u207))
(define-constant ERR-UNAUTHORIZED (err u208))
(define-constant ERR-POOL-FULL (err u209))
(define-constant ERR-INVALID-DURATION (err u210))

;; Data Variables
(define-data-var next-pool-id uint u1)
(define-data-var total-pools-created uint u0)
(define-data-var total-premiums-collected uint u0)
(define-data-var total-active-pools uint u0)
(define-data-var contract-active bool true)

;; Data Maps
(define-map pools
    uint
    {
        creator: principal,
        name: (string-ascii 50),
        description: (string-ascii 300),
        premium-amount: uint,
        max-members: uint,
        current-members: uint,
        total-balance: uint,
        coverage-amount: uint,
        pool-status: (string-ascii 20),
        creation-time: uint,
        expiry-time: uint,
        risk-category: (string-ascii 30),
        minimum-age: uint,
        maximum-age: uint
    }
)

(define-map pool-members
    {pool-id: uint, member: principal}
    {
        join-time: uint,
        premium-paid: uint,
        last-payment: uint,
        policy-active: bool,
        coverage-start: uint,
        coverage-end: uint,
        payment-count: uint
    }
)

(define-map pool-managers uint principal)
(define-map user-pools principal (list 20 uint))
(define-map pool-statistics uint {
    total-premiums: uint,
    total-claims: uint,
    total-payouts: uint,
    member-count: uint,
    active-policies: uint
})

(define-map premium-history
    {pool-id: uint, member: principal}
    (list 50 {amount: uint, timestamp: uint, period-start: uint, period-end: uint})
)

;; Private Functions

;; Validate pool creation data
(define-private (is-valid-pool-data (name (string-ascii 50)) (premium uint) (max-members uint) (coverage uint) (duration uint))
    (and
        (> (len name) u0)
        (> premium u0)
        (> max-members u0)
        (<= max-members u1000)
        (> coverage u0)
        (> duration u0)
        (<= duration u52560000) ;; Max 1 year in blocks (~10 minutes per block)
    )
)

;; Check if pool is active
(define-private (is-pool-active (pool-id uint))
    (match (map-get? pools pool-id)
        pool (and
            (is-eq (get pool-status pool) "active")
            (< block-height (get expiry-time pool))
        )
        false
    )
)

;; Check if user is pool manager
(define-private (is-pool-manager (pool-id uint) (user principal))
    (is-eq (some user) (map-get? pool-managers pool-id))
)

;; Update user pool list
(define-private (update-user-pools (user principal) (pool-id uint))
    (let ((current-pools (default-to (list) (map-get? user-pools user))))
        (if (< (len current-pools) u20)
            (map-set user-pools user (unwrap-panic (as-max-len? (append current-pools pool-id) u20)))
            (map-set user-pools user (unwrap-panic (as-max-len? (append (unwrap-panic (slice? current-pools u1 u20)) pool-id) u20)))
        )
    )
)

;; Calculate coverage end time
(define-private (calculate-coverage-end (start-time uint) (duration-blocks uint))
    (+ start-time duration-blocks)
)

;; Add premium payment to history
(define-private (add-premium-payment (pool-id uint) (member principal) (amount uint) (period-start uint) (period-end uint))
    (let ((current-history (default-to (list) (map-get? premium-history {pool-id: pool-id, member: member}))))
        (if (< (len current-history) u50)
            (map-set premium-history {pool-id: pool-id, member: member}
                (unwrap-panic (as-max-len?
                    (append current-history {amount: amount, timestamp: block-height, period-start: period-start, period-end: period-end})
                    u50)))
            (map-set premium-history {pool-id: pool-id, member: member}
                (unwrap-panic (as-max-len?
                    (append (unwrap-panic (slice? current-history u1 u50))
                        {amount: amount, timestamp: block-height, period-start: period-start, period-end: period-end})
                    u50)))
        )
    )
)

;; Public Functions

;; Create a new insurance pool
(define-public (create-pool
    (name (string-ascii 50))
    (description (string-ascii 300))
    (premium-amount uint)
    (max-members uint)
    (coverage-amount uint)
    (duration-blocks uint)
    (risk-category (string-ascii 30))
    (min-age uint)
    (max-age uint)
)
    (let
        (
            (pool-id (var-get next-pool-id))
            (current-time block-height)
            (expiry-time (+ current-time duration-blocks))
        )
        (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
        (asserts! (is-valid-pool-data name premium-amount max-members coverage-amount duration-blocks) ERR-INVALID-POOL)
        (asserts! (< min-age max-age) ERR-INVALID-POOL)

        (map-set pools pool-id {
            creator: tx-sender,
            name: name,
            description: description,
            premium-amount: premium-amount,
            max-members: max-members,
            current-members: u0,
            total-balance: u0,
            coverage-amount: coverage-amount,
            pool-status: "active",
            creation-time: current-time,
            expiry-time: expiry-time,
            risk-category: risk-category,
            minimum-age: min-age,
            maximum-age: max-age
        })

        (map-set pool-managers pool-id tx-sender)
        (map-set pool-statistics pool-id {
            total-premiums: u0,
            total-claims: u0,
            total-payouts: u0,
            member-count: u0,
            active-policies: u0
        })

        (update-user-pools tx-sender pool-id)
        (var-set next-pool-id (+ pool-id u1))
        (var-set total-pools-created (+ (var-get total-pools-created) u1))
        (var-set total-active-pools (+ (var-get total-active-pools) u1))

        (ok pool-id)
    )
)

;; Join an insurance pool
(define-public (join-pool (pool-id uint) (user-age uint))
    (let
        (
            (pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND))
            (current-time block-height)
            (coverage-duration u2160000) ;; ~1 year in blocks
        )
        (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
        (asserts! (is-pool-active pool-id) ERR-POOL-CLOSED)
        (asserts! (< (get current-members pool) (get max-members pool)) ERR-POOL-FULL)
        (asserts! (is-none (map-get? pool-members {pool-id: pool-id, member: tx-sender})) ERR-ALREADY-MEMBER)
        (asserts! (and (>= user-age (get minimum-age pool)) (<= user-age (get maximum-age pool))) ERR-INVALID-POOL)

        ;; In a real implementation, STX would be transferred here
        ;; For now, we simulate the premium payment
        (map-set pool-members {pool-id: pool-id, member: tx-sender} {
            join-time: current-time,
            premium-paid: (get premium-amount pool),
            last-payment: current-time,
            policy-active: true,
            coverage-start: current-time,
            coverage-end: (calculate-coverage-end current-time coverage-duration),
            payment-count: u1
        })

        ;; Update pool data
        (map-set pools pool-id (merge pool {
            current-members: (+ (get current-members pool) u1),
            total-balance: (+ (get total-balance pool) (get premium-amount pool))
        }))

        ;; Update statistics
        (let ((stats (unwrap-panic (map-get? pool-statistics pool-id))))
            (map-set pool-statistics pool-id (merge stats {
                total-premiums: (+ (get total-premiums stats) (get premium-amount pool)),
                member-count: (+ (get member-count stats) u1),
                active-policies: (+ (get active-policies stats) u1)
            }))
        )

        (add-premium-payment pool-id tx-sender (get premium-amount pool) current-time (calculate-coverage-end current-time coverage-duration))
        (update-user-pools tx-sender pool-id)
        (var-set total-premiums-collected (+ (var-get total-premiums-collected) (get premium-amount pool)))

        (ok true)
    )
)

;; Pay premium for existing policy
(define-public (pay-premium (pool-id uint))
    (let
        (
            (pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND))
            (member-data (unwrap! (map-get? pool-members {pool-id: pool-id, member: tx-sender}) ERR-NOT-MEMBER))
            (current-time block-height)
            (coverage-duration u2160000)
        )
        (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
        (asserts! (is-pool-active pool-id) ERR-POOL-CLOSED)
        (asserts! (get policy-active member-data) ERR-UNAUTHORIZED)

        ;; In a real implementation, STX would be transferred here
        (map-set pool-members {pool-id: pool-id, member: tx-sender} (merge member-data {
            premium-paid: (+ (get premium-paid member-data) (get premium-amount pool)),
            last-payment: current-time,
            coverage-end: (calculate-coverage-end current-time coverage-duration),
            payment-count: (+ (get payment-count member-data) u1)
        }))

        ;; Update pool balance
        (map-set pools pool-id (merge pool {
            total-balance: (+ (get total-balance pool) (get premium-amount pool))
        }))

        ;; Update statistics
        (let ((stats (unwrap-panic (map-get? pool-statistics pool-id))))
            (map-set pool-statistics pool-id (merge stats {
                total-premiums: (+ (get total-premiums stats) (get premium-amount pool))
            }))
        )

        (add-premium-payment pool-id tx-sender (get premium-amount pool) current-time (calculate-coverage-end current-time coverage-duration))
        (var-set total-premiums-collected (+ (var-get total-premiums-collected) (get premium-amount pool)))

        (ok true)
    )
)

;; Leave insurance pool
(define-public (leave-pool (pool-id uint))
    (let
        (
            (pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND))
            (member-data (unwrap! (map-get? pool-members {pool-id: pool-id, member: tx-sender}) ERR-NOT-MEMBER))
        )
        (asserts! (var-get contract-active) ERR-UNAUTHORIZED)
        (asserts! (get policy-active member-data) ERR-UNAUTHORIZED)

        ;; Deactivate policy
        (map-set pool-members {pool-id: pool-id, member: tx-sender} (merge member-data {
            policy-active: false,
            coverage-end: block-height
        }))

        ;; Update pool member count
        (map-set pools pool-id (merge pool {
            current-members: (- (get current-members pool) u1)
        }))

        ;; Update statistics
        (let ((stats (unwrap-panic (map-get? pool-statistics pool-id))))
            (map-set pool-statistics pool-id (merge stats {
                active-policies: (- (get active-policies stats) u1)
            }))
        )

        (ok true)
    )
)

;; Administrative Functions

;; Close pool (only pool manager)
(define-public (close-pool (pool-id uint))
    (let ((pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND)))
        (asserts! (or (is-pool-manager pool-id tx-sender) (is-eq tx-sender CONTRACT-OWNER)) ERR-UNAUTHORIZED)
        (asserts! (is-eq (get pool-status pool) "active") ERR-POOL-CLOSED)

        (map-set pools pool-id (merge pool {pool-status: "closed"}))
        (var-set total-active-pools (- (var-get total-active-pools) u1))

        (ok true)
    )
)

;; Update pool premium (only pool manager)
(define-public (update-premium (pool-id uint) (new-premium uint))
    (let ((pool (unwrap! (map-get? pools pool-id) ERR-POOL-NOT-FOUND)))
        (asserts! (is-pool-manager pool-id tx-sender) ERR-UNAUTHORIZED)
        (asserts! (> new-premium u0) ERR-INVALID-AMOUNT)

        (map-set pools pool-id (merge pool {premium-amount: new-premium}))
        (ok true)
    )
)

;; Transfer pool management (only current manager)
(define-public (transfer-pool-management (pool-id uint) (new-manager principal))
    (begin
        (asserts! (is-pool-manager pool-id tx-sender) ERR-UNAUTHORIZED)
        (map-set pool-managers pool-id new-manager)
        (ok true)
    )
)

;; Toggle contract status (only owner)
(define-public (toggle-contract-status)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
        (var-set contract-active (not (var-get contract-active)))
        (ok (var-get contract-active))
    )
)

;; Read-only Functions

;; Get pool details
(define-read-only (get-pool (pool-id uint))
    (map-get? pools pool-id)
)

;; Get pool member details
(define-read-only (get-pool-member (pool-id uint) (member principal))
    (map-get? pool-members {pool-id: pool-id, member: member})
)

;; Get user pools
(define-read-only (get-user-pools (user principal))
    (default-to (list) (map-get? user-pools user))
)

;; Get pool statistics
(define-read-only (get-pool-statistics (pool-id uint))
    (map-get? pool-statistics pool-id)
)

;; Get premium history
(define-read-only (get-premium-history (pool-id uint) (member principal))
    (default-to (list) (map-get? premium-history {pool-id: pool-id, member: member}))
)

;; Get contract statistics
(define-read-only (get-contract-stats)
    {
        total-pools-created: (var-get total-pools-created),
        total-active-pools: (var-get total-active-pools),
        total-premiums-collected: (var-get total-premiums-collected),
        next-pool-id: (var-get next-pool-id),
        contract-active: (var-get contract-active)
    }
)

;; Check if user is pool member
(define-read-only (is-pool-member (pool-id uint) (user principal))
    (is-some (map-get? pool-members {pool-id: pool-id, member: user}))
)

;; Get pool manager
(define-read-only (get-pool-manager (pool-id uint))
    (map-get? pool-managers pool-id)
)

;; Check if pool is active
(define-read-only (is-active-pool (pool-id uint))
    (is-pool-active pool-id)
)

;; Get contract owner
(define-read-only (get-contract-owner)
    CONTRACT-OWNER
)

;; Check policy status
(define-read-only (get-policy-status (pool-id uint) (member principal))
    (match (map-get? pool-members {pool-id: pool-id, member: member})
        member-data (some (get policy-active member-data))
        none
    )
)
