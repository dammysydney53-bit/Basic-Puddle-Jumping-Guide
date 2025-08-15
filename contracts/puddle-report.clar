;; Puddle Reports Contract
;; Basic puddle jumping coordination system

(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-UNAUTHORIZED (err u403))
(define-constant ERR-INVALID-INPUT (err u400))

(define-map puddle-reports
  { location: (string-ascii 50) }
  {
    depth-cm: uint,
    weather-rating: uint,
    safety-score: uint,
    reporter: principal,
    block-height: uint,
    active: bool
  }
)

(define-map location-activity
  { location: (string-ascii 50) }
  { last-updated: uint, total-reports: uint }
)

(define-public (report-puddle (location (string-ascii 50)) (depth-cm uint) (weather-rating uint) (safety-score uint))
  (let (
    (current-height stacks-block-height)
  )
    (if (and
         (<= depth-cm u100)
         (<= weather-rating u10)
         (<= safety-score u10)
         (> (len location) u0))
      (begin
        (map-set puddle-reports
          { location: location }
          {
            depth-cm: depth-cm,
            weather-rating: weather-rating,
            safety-score: safety-score,
            reporter: tx-sender,
            block-height: current-height,
            active: true
          }
        )
        (map-set location-activity
          { location: location }
          {
            last-updated: current-height,
            total-reports: (+ (get total-reports (default-to { last-updated: u0, total-reports: u0 }
                                                            (map-get? location-activity { location: location }))) u1)
          }
        )
        (ok true)
      )
      ERR-INVALID-INPUT
    )
  )
)

(define-public (deactivate-puddle (location (string-ascii 50)))
  (match (map-get? puddle-reports { location: location })
    report (if (is-eq tx-sender (get reporter report))
             (begin
               (map-set puddle-reports
                 { location: location }
                 (merge report { active: false })
               )
               (ok true)
             )
             ERR-UNAUTHORIZED)
    ERR-NOT-FOUND
  )
)

(define-read-only (get-puddle-report (location (string-ascii 50)))
  (map-get? puddle-reports { location: location })
)

(define-read-only (get-location-stats (location (string-ascii 50)))
  (map-get? location-activity { location: location })
)

(define-read-only (is-suitable-weather (location (string-ascii 50)))
  (match (map-get? puddle-reports { location: location })
    report (and (get active report) (>= (get weather-rating report) u6))
    false
  )
)

(define-read-only (get-safety-rating (location (string-ascii 50)))
  (match (map-get? puddle-reports { location: location })
    report (if (get active report) (some (get safety-score report)) none)
    none
  )
)
