;; Point Exchange Contract
;; Facilitates cross-retailer point exchanges and conversions

;; Error codes
(define-constant ERR-UNAUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-INVALID-RATE (err u400))
(define-constant ERR-INSUFFICIENT-POINTS (err u402))
(define-constant ERR-EXCHANGE-PAUSED (err u403))
(define-constant ERR-INVALID-RETAILER (err u405))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var exchange-active bool true)
(define-data-var base-exchange-rate uint u100) ;; 100 basis points = 1:1 exchange
(define-data-var total-exchanges uint u0)

;; Data maps
(define-map exchange-rates {from-retailer: principal, to-retailer: principal} uint)
(define-map exchange-history {user: principal, exchange-id: uint} {
  from-retailer: principal,
  to-retailer: principal,
  amount-from: uint,
  amount-to: uint,
  timestamp: uint
})
(define-map retailer-exchange-volume principal uint)

;; Read-only functions
(define-read-only (get-exchange-rate (from-retailer principal) (to-retailer principal))
  (default-to (var-get base-exchange-rate) 
    (map-get? exchange-rates {from-retailer: from-retailer, to-retailer: to-retailer}))
)

(define-read-only (get-exchange-history (user principal) (exchange-id uint))
  (map-get? exchange-history {user: user, exchange-id: exchange-id})
)

(define-read-only (get-retailer-volume (retailer principal))
  (default-to u0 (map-get? retailer-exchange-volume retailer))
)

(define-read-only (is-exchange-active)
  (var-get exchange-active)
)

(define-read-only (get-total-exchanges)
  (var-get total-exchanges)
)

(define-read-only (calculate-exchange-amount (amount uint) (from-retailer principal) (to-retailer principal))
  (let ((rate (get-exchange-rate from-retailer to-retailer)))
    (/ (* amount rate) u100)
  )
)

;; Private functions
(define-private (is-owner)
  (is-eq tx-sender (var-get contract-owner))
)

(define-private (increment-exchange-counter)
  (let ((current-count (var-get total-exchanges)))
    (var-set total-exchanges (+ current-count u1))
    current-count
  )
)

;; Public functions
(define-public (set-exchange-rate (from-retailer principal) (to-retailer principal) (rate uint))
  (begin
    (asserts! (is-owner) ERR-UNAUTHORIZED)
    (asserts! (> rate u0) ERR-INVALID-RATE)
    (asserts! (<= rate u200) ERR-INVALID-RATE) ;; Max 2:1 exchange rate
    (ok (map-set exchange-rates 
      {from-retailer: from-retailer, to-retailer: to-retailer} 
      rate))
  )
)

(define-public (toggle-exchange)
  (begin
    (asserts! (is-owner) ERR-UNAUTHORIZED)
    (ok (var-set exchange-active (not (var-get exchange-active))))
  )
)

(define-public (exchange-points (from-retailer principal) (to-retailer principal) (amount uint))
  (let ((exchange-rate (get-exchange-rate from-retailer to-retailer))
        (converted-amount (calculate-exchange-amount amount from-retailer to-retailer))
        (exchange-id (increment-exchange-counter))
        (current-block stacks-block-height))
    (asserts! (var-get exchange-active) ERR-EXCHANGE-PAUSED)
    (asserts! (> amount u0) ERR-INSUFFICIENT-POINTS)
    (asserts! (not (is-eq from-retailer to-retailer)) ERR-INVALID-RETAILER)
    
    ;; Record exchange history
    (map-set exchange-history {user: tx-sender, exchange-id: exchange-id} {
      from-retailer: from-retailer,
      to-retailer: to-retailer,
      amount-from: amount,
      amount-to: converted-amount,
      timestamp: current-block
    })
    
    ;; Update volume tracking
    (map-set retailer-exchange-volume from-retailer 
      (+ (get-retailer-volume from-retailer) amount))
    (map-set retailer-exchange-volume to-retailer 
      (+ (get-retailer-volume to-retailer) converted-amount))
    
    (ok {
      exchange-id: exchange-id,
      amount-exchanged: converted-amount,
      rate-used: exchange-rate
    })
  )
)

(define-public (batch-set-rates (rate-list (list 10 {from: principal, to: principal, rate: uint})))
  (begin
    (asserts! (is-owner) ERR-UNAUTHORIZED)
    (ok (map set-single-rate rate-list))
  )
)

(define-private (set-single-rate (rate-data {from: principal, to: principal, rate: uint}))
  (map-set exchange-rates 
    {from-retailer: (get from rate-data), to-retailer: (get to rate-data)} 
    (get rate rate-data))
)

(define-public (update-base-rate (new-rate uint))
  (begin
    (asserts! (is-owner) ERR-UNAUTHORIZED)
    (asserts! (> new-rate u0) ERR-INVALID-RATE)
    (asserts! (<= new-rate u200) ERR-INVALID-RATE)
    (ok (var-set base-exchange-rate new-rate))
  )
)

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-owner) ERR-UNAUTHORIZED)
    (ok (var-set contract-owner new-owner))
  )
)

