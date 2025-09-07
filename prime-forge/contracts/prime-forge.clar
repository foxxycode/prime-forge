;; Prime Forge Dual-Collateral Algorithmic Stablecoin Protocol

;; Error Constants
(define-constant ERR-NOT-AUTHORIZED (err u1000))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1001))
(define-constant ERR-INVALID-AMOUNT (err u1002))
(define-constant ERR-PROTOCOL-PAUSED (err u1003))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u1004))
(define-constant ERR-INVALID-PRIME-SCORE (err u1005))
(define-constant ERR-MARKET-VOLATILITY-HIGH (err u1006))
(define-constant ERR-CIRCUIT-BREAKER-ACTIVE (err u1007))
(define-constant ERR-INVALID-VAULT-ID (err u1008))
(define-constant ERR-USER-NOT-FOUND (err u1009))
(define-constant ERR-TIMELOCK-ACTIVE (err u1010))
(define-constant ERR-INVALID-GOVERNANCE-PROPOSAL (err u1011))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MIN-COLLATERAL-RATIO u150) ;; 150%
(define-constant MAX-PRIME-SCORE u1000)
(define-constant VOLATILITY-THRESHOLD u500) ;; 5%
(define-constant CIRCUIT-BREAKER-THRESHOLD u2000) ;; 20%
(define-constant TIMELOCK-PERIOD u1440) ;; 24 hours in blocks

;; Data Variables
(define-data-var protocol-paused bool false)
(define-data-var total-prime-supply uint u0)
(define-data-var total-forge-supply uint u0)
(define-data-var total-flux-supply uint u0)
(define-data-var current-volatility uint u0)
(define-data-var algorithmic-mode bool false)
(define-data-var circuit-breaker-active bool false)
(define-data-var last-stability-check uint u0)
(define-data-var base-collateral-ratio uint u150)
(define-data-var emergency-admin (optional principal) none)
(define-data-var governance-timelock uint u0)

;; Data Maps
(define-map user-prime-scores principal uint)
(define-map user-balances-prime principal uint)
(define-map user-balances-forge principal uint)
(define-map user-balances-flux principal uint)
(define-map user-staking-history principal {total-staked: uint, stake-duration: uint, last-stake-block: uint})
(define-map user-collateral-positions principal {collateral-amount: uint, debt-amount: uint, collateral-ratio: uint})
(define-map dynamic-yield-vaults uint {owner: principal, balance: uint, strategy: (string-ascii 50), last-rebalance: uint, yield-rate: uint})
(define-map vault-counter principal uint)
(define-map market-volatility-data uint {volatility-score: uint, timestamp: uint, market-cap: uint})
(define-map governance-proposals uint {proposer: principal, description: (string-ascii 500), votes-for: uint, votes-against: uint, executed: bool})
(define-map user-governance-power principal uint)

;; Authorization Functions
(define-private (is-contract-owner)
    (is-eq tx-sender CONTRACT-OWNER))

(define-private (is-emergency-admin)
    (match (var-get emergency-admin)
        admin (is-eq tx-sender admin)
        false))

(define-private (is-authorized-admin)
    (or (is-contract-owner) (is-emergency-admin)))

;; Input Validation Functions
(define-private (validate-amount (amount uint))
    (> amount u0))

(define-private (validate-principal (user principal))
    (not (is-eq user CONTRACT-OWNER)))

(define-private (check-protocol-status)
    (and (not (var-get protocol-paused)) 
         (not (var-get circuit-breaker-active))))

;; Prime Score Calculation
(define-private (calculate-prime-score (user principal))
    (let (
        (staking-data (default-to {total-staked: u0, stake-duration: u0, last-stake-block: u0} 
                                 (map-get? user-staking-history user)))
        (governance-power (default-to u0 (map-get? user-governance-power user)))
        (base-score u100)
        (staking-bonus (/ (get total-staked staking-data) u1000))
        (duration-bonus (/ (get stake-duration staking-data) u100))
        (governance-bonus (/ governance-power u10))
    )
    (min (+ base-score staking-bonus duration-bonus governance-bonus) MAX-PRIME-SCORE)))

;; Volatility Analysis
(define-private (analyze-market-volatility)
    (let (
        (current-block block-height)
        (last-check (var-get last-stability-check))
        (volatility-increase (> (- current-block last-check) u100))
    )
    (if volatility-increase
        (let (
            (new-volatility (+ (var-get current-volatility) u50))
        )
        (var-set current-volatility new-volatility)
        (var-set last-stability-check current-block)
        (if (> new-volatility CIRCUIT-BREAKER-THRESHOLD)
            (var-set circuit-breaker-active true)
            true))
        true)))

;; Dynamic Collateral Ratio Calculation
(define-private (calculate-dynamic-collateral-ratio (user principal))
    (let (
        (prime-score (calculate-prime-score user))
        (base-ratio (var-get base-collateral-ratio))
        (volatility (var-get current-volatility))
        (score-adjustment (/ (* prime-score u50) MAX-PRIME-SCORE))
        (volatility-adjustment (/ volatility u10))
    )
    (+ (- base-ratio score-adjustment) volatility-adjustment)))

;; Admin Functions
(define-public (set-emergency-admin (new-admin principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (var-set emergency-admin (some new-admin))
        (ok true)))

(define-public (pause-protocol)
    (begin
        (asserts! (is-authorized-admin) ERR-NOT-AUTHORIZED)
        (var-set protocol-paused true)
        (ok true)))

(define-public (unpause-protocol)
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (var-set protocol-paused false)
        (var-set circuit-breaker-active false)
        (ok true)))

(define-public (update-base-collateral-ratio (new-ratio uint))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (>= new-ratio u100) ERR-INVALID-AMOUNT)
        (asserts! (is-eq (var-get governance-timelock) u0) ERR-TIMELOCK-ACTIVE)
        (var-set base-collateral-ratio new-ratio)
        (ok true)))

(define-public (activate-circuit-breaker)
    (begin
        (asserts! (is-authorized-admin) ERR-NOT-AUTHORIZED)
        (var-set circuit-breaker-active true)
        (var-set protocol-paused true)
        (ok true)))

;; Core Protocol Functions
(define-public (mint-prime (collateral-amount uint))
    (let (
        (user tx-sender)
        (prime-score (calculate-prime-score user))
        (required-ratio (calculate-dynamic-collateral-ratio user))
        (mint-amount (/ (* collateral-amount u100) required-ratio))
    )
    (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
    (asserts! (validate-amount collateral-amount) ERR-INVALID-AMOUNT)
    (asserts! (>= collateral-amount (* mint-amount required-ratio)) ERR-INSUFFICIENT-COLLATERAL)
    
    (analyze-market-volatility)
    
    (map-set user-balances-prime user 
             (+ (default-to u0 (map-get? user-balances-prime user)) mint-amount))
    (map-set user-collateral-positions user 
             {collateral-amount: collateral-amount, 
              debt-amount: mint-amount, 
              collateral-ratio: required-ratio})
    (var-set total-prime-supply (+ (var-get total-prime-supply) mint-amount))
    
    (ok mint-amount)))

(define-public (redeem-prime (prime-amount uint))
    (let (
        (user tx-sender)
        (user-balance (default-to u0 (map-get? user-balances-prime user)))
        (position (map-get? user-collateral-positions user))
    )
    (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
    (asserts! (validate-amount prime-amount) ERR-INVALID-AMOUNT)
    (asserts! (>= user-balance prime-amount) ERR-INSUFFICIENT-BALANCE)
    (asserts! (is-some position) ERR-USER-NOT-FOUND)
    
    (let (
        (collateral-to-return (/ (* prime-amount (get collateral-amount (unwrap-panic position))) 
                                (get debt-amount (unwrap-panic position))))
    )
    (map-set user-balances-prime user (- user-balance prime-amount))
    (var-set total-prime-supply (- (var-get total-prime-supply) prime-amount))
    
    (ok collateral-to-return))))

(define-public (stake-forge (amount uint))
    (let (
        (user tx-sender)
        (current-balance (default-to u0 (map-get? user-balances-forge user)))
        (current-staking (default-to {total-staked: u0, stake-duration: u0, last-stake-block: u0} 
                                    (map-get? user-staking-history user)))
    )
    (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
    (asserts! (validate-amount amount) ERR-INVALID-AMOUNT)
    (asserts! (>= current-balance amount) ERR-INSUFFICIENT-BALANCE)
    
    (map-set user-balances-forge user (- current-balance amount))
    (map-set user-staking-history user 
             {total-staked: (+ (get total-staked current-staking) amount),
              stake-duration: (+ (get stake-duration current-staking) u1),
              last-stake-block: block-height})
    
    ;; Update Prime Score after staking
    (map-set user-prime-scores user (calculate-prime-score user))
    
    (ok true)))

(define-public (create-dynamic-vault (initial-deposit uint) (strategy (string-ascii 50)))
    (let (
        (user tx-sender)
        (vault-id (+ (default-to u0 (map-get? vault-counter user)) u1))
        (user-balance (default-to u0 (map-get? user-balances-prime user)))
    )
    (asserts! (check-protocol-status) ERR-PROTOCOL-PAUSED)
    (asserts! (validate-amount initial-deposit) ERR-INVALID-AMOUNT)
    (asserts! (>= user-balance initial-deposit