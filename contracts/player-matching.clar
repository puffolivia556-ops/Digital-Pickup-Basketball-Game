;; Player Matching Contract
;; Manages player profiles, skill levels, and game matching

;; Constants
(define-constant err-player-not-found (err u200))
(define-constant err-invalid-skill-level (err u201))
(define-constant err-game-not-found (err u202))
(define-constant err-game-full (err u203))

;; Data Variables
(define-data-var next-game-id uint u1)

;; Data Maps
(define-map players principal {
  skill-level: uint,
  games-played: uint,
  rating: uint,
  registered-at: uint
})

(define-map games uint {
  court-id: uint,
  organizer: principal,
  skill-range-min: uint,
  skill-range-max: uint,
  max-players: uint,
  current-players: uint,
  start-time: uint,
  status: (string-ascii 20)
})

(define-map game-players {game-id: uint, player: principal} bool)

;; Public Functions
(define-public (register-player (skill-level uint))
  (begin
    (asserts! (and (>= skill-level u1) (<= skill-level u10)) err-invalid-skill-level)
    (map-set players tx-sender {
      skill-level: skill-level,
      games-played: u0,
      rating: (* skill-level u100),
      registered-at: burn-block-height
    })
    (ok true)))

(define-public (create-game (court-id uint) (skill-min uint) (skill-max uint) (max-players uint) (start-time uint))
  (let ((game-id (var-get next-game-id)))
    (asserts! (and (>= skill-min u1) (<= skill-max u10)) err-invalid-skill-level)
    (asserts! (<= skill-min skill-max) err-invalid-skill-level)
    (map-set games game-id {
      court-id: court-id,
      organizer: tx-sender,
      skill-range-min: skill-min,
      skill-range-max: skill-max,
      max-players: max-players,
      current-players: u1,
      start-time: start-time,
      status: "open"
    })
    (map-set game-players {game-id: game-id, player: tx-sender} true)
    (var-set next-game-id (+ game-id u1))
    (ok game-id)))

(define-public (join-game (game-id uint))
  (let ((game (unwrap! (map-get? games game-id) err-game-not-found))
        (player (unwrap! (map-get? players tx-sender) err-player-not-found)))
    (asserts! (< (get current-players game) (get max-players game)) err-game-full)
    (asserts! (and (>= (get skill-level player) (get skill-range-min game))
                   (<= (get skill-level player) (get skill-range-max game))) err-invalid-skill-level)

    (map-set game-players {game-id: game-id, player: tx-sender} true)
    (map-set games game-id (merge game {current-players: (+ (get current-players game) u1)}))
    (ok true)))

;; Read-only Functions
(define-read-only (get-player (player principal))
  (map-get? players player))

(define-read-only (get-game (game-id uint))
  (map-get? games game-id))

(define-read-only (is-player-in-game (game-id uint) (player principal))
  (default-to false (map-get? game-players {game-id: game-id, player: player})))

(define-read-only (find-matching-games (player principal))
  (let ((player-data (unwrap! (map-get? players player) err-player-not-found)))
    (ok (get skill-level player-data))))
