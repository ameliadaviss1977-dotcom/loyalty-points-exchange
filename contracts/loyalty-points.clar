;; Loyalty Points Contract
;; Manages loyalty points for customers across different retailers

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INSUFFICIENT-BALANCE (err u400))
(define-constant ERR-INVALID-AMOUNT (err u402))
(define-constant ERR-RETAILER-NOT-REGISTERED (err u403))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-points uint u0)

;; Data maps
(define-map user-balances principal uint)
(define-map retailer-registry principal {
  name: (string-ascii 50),
  is-active: bool,
  points-issued: uint
})
(define-map user-retailer-points {user: principal, retailer: principal} uint)

;; Read-only functions
(define-read-only (get-balance (user principal))
  (default-to u0 (map-get? user-balances user))
)

(define-read-only (get-retailer-info (retailer principal))
  (map-get? retailer-registry retailer)
)

(define-read-only (get-user-retailer-points (user principal) (retailer principal))
  (default-to u0 (map-get? user-retailer-points {user: user, retailer: retailer}))
)

(define-read-only (get-total-points)
  (var-get total-points)
)

(define-read-only (get-contract-owner)
  (var-get contract-owner)
)

;; Private functions
(define-private (is-authorized (user principal))
  (or (is-eq user (var-get contract-owner))
      (is-some (get-retailer-info user))
  )
)

;; Public functions
(define-public (register-retailer (retailer principal) (name (string-ascii 50)))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (asserts! (> (len name) u0) ERR-INVALID-AMOUNT)
    (ok (map-set retailer-registry retailer {
      name: name,
      is-active: true,
      points-issued: u0
    }))
  )
)

(define-public (deactivate-retailer (retailer principal))
  (let ((retailer-data (unwrap! (get-retailer-info retailer) ERR-RETAILER-NOT-REGISTERED)))
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (map-set retailer-registry retailer 
      (merge retailer-data {is-active: false})
    ))
  )
)

(define-public (issue-points (user principal) (amount uint))
  (let ((retailer-data (unwrap! (get-retailer-info tx-sender) ERR-RETAILER-NOT-REGISTERED))
        (current-balance (get-balance user))
        (current-retailer-points (get-user-retailer-points user tx-sender)))
    (asserts! (get is-active retailer-data) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    ;; Update user's total balance
    (map-set user-balances user (+ current-balance amount))
    ;; Update user's points from this retailer
    (map-set user-retailer-points {user: user, retailer: tx-sender} 
             (+ current-retailer-points amount))
    ;; Update retailer's total points issued
    (map-set retailer-registry tx-sender 
      (merge retailer-data {points-issued: (+ (get points-issued retailer-data) amount)}))
    ;; Update total points in circulation
    (var-set total-points (+ (var-get total-points) amount))
    (ok amount)
  )
)

(define-public (transfer-points (recipient principal) (amount uint))
  (let ((sender-balance (get-balance tx-sender)))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
    ;; Deduct from sender
    (map-set user-balances tx-sender (- sender-balance amount))
    ;; Add to recipient
    (map-set user-balances recipient (+ (get-balance recipient) amount))
    (ok amount)
  )
)

(define-public (redeem-points (amount uint) (retailer principal))
  (let ((user-balance (get-balance tx-sender))
        (retailer-data (unwrap! (get-retailer-info retailer) ERR-RETAILER-NOT-REGISTERED)))
    (asserts! (get is-active retailer-data) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= user-balance amount) ERR-INSUFFICIENT-BALANCE)
    ;; Deduct points from user
    (map-set user-balances tx-sender (- user-balance amount))
    ;; Update total points in circulation
    (var-set total-points (- (var-get total-points) amount))
    (ok amount)
  )
)

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)
