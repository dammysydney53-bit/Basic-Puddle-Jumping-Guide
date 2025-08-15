;; Activity Coordinator Contract
;; Coordinates puddle jumping activities and safety guidelines

(define-constant ERR-INVALID-RATING (err u400))
(define-constant ERR-UNSAFE-CONDITIONS (err u403))

(define-map activity-sessions
  { session-id: uint }
  {
    location: (string-ascii 50),
    organizer: principal,
    min-safety-score: uint,
    max-participants: uint,
    current-participants: uint,
    active: bool,
    created-at: uint
  }
)

(define-data-var session-counter uint u0)

(define-public (create-activity-session
  (location (string-ascii 50))
  (min-safety-score uint)
  (max-participants uint))
  (let (
    (session-id (+ (var-get session-counter) u1))
    (current-height block-height)
  )
    (if (and (<= min-safety-score u10) (> max-participants u0))
      (begin
        (map-set activity-sessions
          { session-id: session-id }
          {
            location: location,
            organizer: tx-sender,
            min-safety-score: min-safety-score,
            max-participants: max-participants,
            current-participants: u0,
            active: true,
            created-at: current-height
          }
        )
        (var-set session-counter session-id)
        (ok session-id)
      )
      ERR-INVALID-RATING
    )
  )
)

(define-public (join-session (session-id uint))
  (match (map-get? activity-sessions { session-id: session-id })
    session (if (and
                 (get active session)
                 (< (get current-participants session) (get max-participants session)))
              (begin
                (map-set activity-sessions
                  { session-id: session-id }
                  (merge session { current-participants: (+ (get current-participants session) u1) })
                )
                (ok true)
              )
              ERR-UNSAFE-CONDITIONS)
    ERR-INVALID-RATING
  )
)

(define-read-only (get-session (session-id uint))
  (map-get? activity-sessions { session-id: session-id })
)

(define-read-only (check-location-safety (location (string-ascii 50)) (required-safety uint))
  (match (contract-call? .puddle-reports get-safety-rating location)
    (some safety-score) (>= safety-score required-safety)
    false
  )
)

(define-read-only (get-weather-suitability (location (string-ascii 50)))
  (contract-call? .puddle-reports is-suitable-weather location)
)
