;; Court Booking Contract
;; Manages court availability and basic booking functionality

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-court-not-found (err u101))
(define-constant err-court-unavailable (err u102))
(define-constant err-invalid-time (err u103))
(define-constant err-booking-not-found (err u104))

;; Data Variables
(define-data-var next-court-id uint u1)
(define-data-var next-booking-id uint u1)

;; Data Maps
(define-map courts uint {
  name: (string-ascii 50),
  location: (string-ascii 100),
  available: bool
})

(define-map bookings uint {
  court-id: uint,
  player: principal,
  start-time: uint,
  end-time: uint,
  created-at: uint
})

(define-map court-schedule {court-id: uint, time-slot: uint} uint)

;; Public Functions
(define-public (add-court (name (string-ascii 50)) (location (string-ascii 100)))
  (let ((court-id (var-get next-court-id)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set courts court-id {
      name: name,
      location: location,
      available: true
    })
    (var-set next-court-id (+ court-id u1))
    (ok court-id)))

(define-public (book-court (court-id uint) (start-time uint) (end-time uint))
  (let ((booking-id (var-get next-booking-id))
        (current-block burn-block-height))
    (asserts! (is-some (map-get? courts court-id)) err-court-not-found)
    (asserts! (< start-time end-time) err-invalid-time)
    (asserts! (>= start-time current-block) err-invalid-time)
    (asserts! (is-court-available court-id start-time end-time) err-court-unavailable)

    (map-set bookings booking-id {
      court-id: court-id,
      player: tx-sender,
      start-time: start-time,
      end-time: end-time,
      created-at: current-block
    })
    (map-set court-schedule {court-id: court-id, time-slot: start-time} booking-id)
    (var-set next-booking-id (+ booking-id u1))
    (ok booking-id)))

(define-public (cancel-booking (booking-id uint))
  (let ((booking (unwrap! (map-get? bookings booking-id) err-booking-not-found)))
    (asserts! (is-eq tx-sender (get player booking)) err-owner-only)
    (map-delete court-schedule {court-id: (get court-id booking), time-slot: (get start-time booking)})
    (map-delete bookings booking-id)
    (ok true)))

;; Read-only Functions
(define-read-only (get-court (court-id uint))
  (map-get? courts court-id))

(define-read-only (get-booking (booking-id uint))
  (map-get? bookings booking-id))

(define-read-only (is-court-available (court-id uint) (start-time uint) (end-time uint))
  (let ((court (unwrap! (map-get? courts court-id) false)))
    (and
      (get available court)
      (is-none (map-get? court-schedule {court-id: court-id, time-slot: start-time})))))

(define-read-only (get-player-bookings (player principal))
  (ok player))
