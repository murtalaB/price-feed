;; Price Feed Contract
;; This contract provides a decentralized price feed service for various assets

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PRICE (err u101))
(define-constant ERR-UNKNOWN-ASSET (err u102))
(define-constant ERR-STALE-PRICE (err u103))
(define-constant ERR-PRICE-DIVERGENCE (err u104))
(define-constant ERR-INVALID-PARAMETER (err u105))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var last-feed-update uint u0)
(define-data-var max-price-age uint u3600) ;; Maximum age of a price in seconds (1 hour)
(define-data-var price-divergence-limit uint u1000) ;; 10% expressed as basis points (100 = 1%)

;; Validate function parameters
(define-private (validate-new-owner (new-owner principal))
  (is-eq tx-sender (var-get contract-owner)))

;; Allow contract owner to be changed
(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (validate-new-owner new-owner) ERR-NOT-AUTHORIZED)
    (ok (var-set contract-owner new-owner))))

;; Validate max price age
(define-private (validate-max-age (new-max-age uint))
  (and 
    (is-eq tx-sender (var-get contract-owner))
    (> new-max-age u0)))

;; Allow setting max price age
(define-public (set-max-price-age (new-max-age uint))
  (begin
    (asserts! (validate-max-age new-max-age) ERR-INVALID-PARAMETER)
    (ok (var-set max-price-age new-max-age))))

;; Validate price divergence limit
(define-private (validate-price-divergence-limit (new-limit uint))
  (and 
    (is-eq tx-sender (var-get contract-owner))
    (>= new-limit u100)  ;; At least 1%
    (<= new-limit u5000))) ;; At most 50%

;; Allow setting price divergence limit
(define-public (set-price-divergence-limit (new-limit uint))
  (begin
    (asserts! (validate-price-divergence-limit new-limit) ERR-INVALID-PARAMETER)
    (ok (var-set price-divergence-limit new-limit))))

;; Map to track authorized price providers
(define-map authorized-providers principal bool)

;; Validate provider address
(define-private (validate-provider (provider principal))
  (and 
    (is-eq tx-sender (var-get contract-owner))
    (not (is-eq provider tx-sender)))) ;; Contract owner is already a provider

;; Add a price provider
(define-public (add-provider (provider principal))
  (begin
    (asserts! (validate-provider provider) ERR-INVALID-PARAMETER)
    (ok (map-set authorized-providers provider true))))

;; Remove a price provider
(define-public (remove-provider (provider principal))
  (begin
    (asserts! (and 
      (is-eq tx-sender (var-get contract-owner))
      (not (is-eq provider (var-get contract-owner)))) ;; Can't remove contract owner
      ERR-INVALID-PARAMETER)
    (ok (map-delete authorized-providers provider))))

;; Check if a principal is an authorized provider
(define-read-only (is-authorized-provider (provider principal))
  (default-to false (map-get? authorized-providers provider)))

;; Asset price structure
(define-map asset-prices
  { asset: (string-ascii 24) }
  { 
    price: uint,          ;; Price in USD with 8 decimal places (e.g., 1 USD = 100000000)
    decimals: uint,       ;; Number of decimal places for the price
    last-update: uint,    ;; Block height of the last update
    last-timestamp: uint, ;; UNIX timestamp of the last update
    provider: principal   ;; Principal who provided the last price update
  }
)

;; Asset price history
(define-map price-history
  { asset: (string-ascii 24), update-id: uint }
  {
    price: uint,
    timestamp: uint,
    provider: principal
  }
)

;; Current update ID counter for each asset
(define-map update-counters { asset: (string-ascii 24) } { counter: uint })

;; Validate asset string
(define-private (validate-asset (asset (string-ascii 24)))
  (> (len asset) u0))

;; Get current update counter for an asset
(define-read-only (get-update-counter (asset (string-ascii 24)))
  (default-to { counter: u0 } (map-get? update-counters { asset: asset })))

;; Increment update counter for an asset
(define-private (increment-update-counter (asset (string-ascii 24)))
  (let ((current-counter (get counter (get-update-counter asset))))
    (map-set update-counters 
      { asset: asset } 
      { counter: (+ u1 current-counter) })))

;; Validate price update parameters
(define-private (validate-price-update (asset (string-ascii 24)) (price uint) (decimals uint))
  (and 
    (is-authorized-provider tx-sender)
    (validate-asset asset)
    (> price u0)
    (<= decimals u18)))

;; Update price for an asset
(define-public (update-price (asset (string-ascii 24)) (price uint) (decimals uint))
  (let (
    (current-block-height block-height)
    (current-time (unwrap! (get-block-info? time current-block-height) ERR-INVALID-PRICE))
    (current-price-data (map-get? asset-prices { asset: asset }))
    (update-counter (get-update-counter asset))
  )
    ;; Check that parameters are valid
    (asserts! (validate-price-update asset price decimals) ERR-INVALID-PARAMETER)
    
    ;; Check for excessive price divergence if there's existing data
    (if (is-some current-price-data)
      (let (
        (existing-price (get price (unwrap-panic current-price-data)))
        (price-change-pct (calculate-price-change-percentage existing-price price))
      )
        ;; Only check divergence if both prices have the same decimal representation
        (if (is-eq decimals (get decimals (unwrap-panic current-price-data)))
          (asserts! (< price-change-pct (var-get price-divergence-limit)) ERR-PRICE-DIVERGENCE)
          true) ;; Different decimal representations, skip the check
      )
      true) ;; No previous price, skip the check
    
    ;; Store the new price
    (map-set asset-prices
      { asset: asset }
      {
        price: price,
        decimals: decimals,
        last-update: current-block-height,
        last-timestamp: current-time,
        provider: tx-sender
      }
    )
    
    ;; Record in price history - counter has been validated
    (map-set price-history
      { asset: asset, update-id: (get counter update-counter) }
      {
        price: price,
        timestamp: current-time,
        provider: tx-sender
      }
    )
    
    ;; Increment counter - asset has been validated
    (increment-update-counter asset)
    
    ;; Update feed timestamp
    (var-set last-feed-update current-time)
    
    (ok price)
  )
)

;; Calculate the percentage change between two prices (in basis points, 1% = 100)
(define-private (calculate-price-change-percentage (old-price uint) (new-price uint))
  (let (
    (price-diff (if (> new-price old-price) 
                   (- new-price old-price) 
                   (- old-price new-price)))
    (price-change-pct (/ (* price-diff u10000) old-price))
  )
    price-change-pct
  )
)

;; Get the latest price for an asset
(define-read-only (get-price (asset (string-ascii 24)))
  (let ((price-data (map-get? asset-prices { asset: asset })))
    (if (is-some price-data)
      (let (
        (price-info (unwrap-panic price-data))
        (current-time (unwrap! (get-block-info? time block-height) ERR-INVALID-PRICE))
        (time-since-update (- current-time (get last-timestamp price-info)))
      )
        ;; Check if price is stale
        (if (> time-since-update (var-get max-price-age))
          ERR-STALE-PRICE
          (ok {
            price: (get price price-info),
            decimals: (get decimals price-info),
            last-update: (get last-update price-info),
            last-timestamp: (get last-timestamp price-info),
            provider: (get provider price-info)
          })
        )
      )
      ERR-UNKNOWN-ASSET
    )
  )
)

;; Get historical price for an asset at a specific update ID
(define-read-only (get-historical-price (asset (string-ascii 24)) (update-id uint))
  (let ((history-data (map-get? price-history { asset: asset, update-id: update-id })))
    (if (is-some history-data)
      (ok (unwrap-panic history-data))
      ERR-UNKNOWN-ASSET
    )
  )
)

;; Check if a price is stale
(define-read-only (is-price-stale (asset (string-ascii 24)))
  (let ((price-data (map-get? asset-prices { asset: asset })))
    (if (is-some price-data)
      (let (
        (price-info (unwrap-panic price-data))
        (current-block (get-block-info? time block-height))
      )
        (if (is-some current-block)
          (let (
            (current-time (unwrap-panic current-block))
            (time-since-update (- current-time (get last-timestamp price-info)))
          )
            (> time-since-update (var-get max-price-age))
          )
          true  ;; If we can't get the current time, consider the price stale
        )
      )
      true  ;; If no price data exists, consider it stale
    )
  )
)

;; Get the last feed update timestamp
(define-read-only (get-last-feed-update)
  (var-get last-feed-update))

;; Get count of historical prices for an asset
(define-read-only (get-price-history-count (asset (string-ascii 24)))
  (get counter (get-update-counter asset)))

;; Initialize contract
(define-private (initialize)
  (begin
    (var-set contract-owner tx-sender)
    (map-set authorized-providers tx-sender true)
    true))

;; Call initialize on contract deployment
(initialize)