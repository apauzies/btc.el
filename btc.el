;;; btc.el --- Bitcoin related functions

;; Copyright (C) 2020 by Alexandre Pauzies

;; Author: Alexandre Pauzies <alexandre@pauzies.com>
;; URL: https://github.com/apauzies/btc
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Call `btc-usd' to display the value of 1 BTC in USD.

;; Call  `gdax-connect' to follow prices in real time
;; it will create 2 buffers:
;; *gdax-execs* were we persist execs
;; *gdax* to visualize execs
;; Call `gdax-disconnect' to stop

;;; Code:

(require 'request)
(require 'json-pointer)

(defun btc-usd ()
  "Display the latest BTC value in USD."
  (interactive)
  (request
   "https://api.coinbase.com/v2/prices/BTC-USD/spot"
   :parser 'json-read
   :success (function*
             (lambda (&key data &allow-other-keys)
	       (let ((btc-value (string-to-number (json-pointer-get data "/data/amount"))))
		 (message "BTC: $%0.2f" btc-value))))))

;; (run-at-time "1 min" 60 'btc-usd)

(require 'websocket)
(require 'json)
(require 'cl)
(require 'all-the-icons)

(setq gdax-msgs nil
      gdax-websocket nil
      gdax-high 0
      gdax-low 0
      gdax-last 0
      gdax-dir 0)

(setq subscribe-request (json-encode '((type . subscribe)
                                       (product_ids BTC-USD)
				       (channels . (name ticker)))))

(defun gdax-record-high-low (exec)
  "Record last time, high and low values of EXEC."
  (let ((val (gdax-exec-value exec)))
    (if (equal gdax-last val)
        nil
      (progn
        (if (> val gdax-high)
            (setq gdax-high val)
          (if (or (equal 0 gdax-low)
                  (<  val gdax-low))
              (setq gdax-low val)))
        (setq gdax-dir (- val gdax-last))
        (setq gdax-last val)))))

(defun gdax-dir-icon (dir)
  "Insert an icon based on DIR where the market is moving."
  (if (equal 0 dir)
      ""
    (let ((icon (all-the-icons-octicon (if (> dir 0) "arrow-up" "arrow-down")))
          (color (if (> dir 0) "green" "red")))
      (propertize icon 'face (list :foreground color)))))

(defun gdax-print (exec)
  "Print the high, low and last values of EXEC."
  (insert (format "%s, Last: $%0.2f, Size: %0.06f, High: $%0.2f, Low: $%0.2f "
                  (format-time-string "%Y%m%d %T.%3N" (gdax-exec-time exec))
                  (gdax-exec-value exec)
                  (gdax-exec-size exec)
                  gdax-high
                  gdax-low)
                  (gdax-dir-icon gdax-dir) "\n"))

;; (gdax-record 42)
;; (gdax-print)

;; , Last: $9149.99, High: $9166.70, Low: $9014.99 
;; (current-time)
;; (setq mydate (date-to-time "2017-11-26T06:19:26.332000Z"))
;; (setq mydate (time-add mydate (seconds-to-time 0.332000)))
;; (parse-time-string "2017-11-26T06:19:26.332000Z")
;; (format-time-string "%Y%m%d %T.%3N" mydate)

(cl-defstruct gdax-exec
  time
  value
  size
  side)

(defun gdax-persist-exec (msg)
  "Persist a gdax exec MSG."
  (set-buffer (get-buffer-create "*gdax-execs*"))
  (insert (format "%s\n" msg)))

;; (setq raw "{\"side\":\"sell\",\"size\":\"0.49913843\",\"price\":\"9680.00000000\",\"time\":\"2017-11-27T03:15:57.876000Z\"}")
;; (setq myexec (gdax-to-exec (json-read-from-string raw)))
;; (gdax-process-exec myexec)

(defun gdax-to-exec (json)
  "Convert a gdax JSON match msg to exec."
    (make-gdax-exec
     :time (current-time)
     :value (string-to-number (rest (assoc 'price json)))
     :size (string-to-number (rest (assoc 'last_size json)))
     :side (rest (assoc 'side json))))

(defun gdax-process-exec (exec)
  "Process a gdax EXEC."
  (progn
    (gdax-record-high-low exec)
    (set-buffer (get-buffer-create "*gdax*"))
    (gdax-print exec)
    (gdax-persist-exec exec)))

(defun gdax-connect ()
  "Connect to gdax api and subscribe to BTC-USD."
  (interactive)
  (if gdax-websocket
      (message "Already connected!")
    (websocket-open
     "wss://ws-feed.gdax.com"
     :on-open (lambda (_websocket)
                (message "Websocket opened")
                (setq gdax-websocket _websocket)
                (websocket-send-text _websocket subscribe-request))
     :on-message (lambda (_websocket frame)
                   ;; (push (websocket-frame-text frame) gdax-msgs)
                   (let* ((data (websocket-frame-text frame))
                          (json (json-read-from-string data))
                          (type (rest (assoc 'type json))))
                     (cond ((equal "ticker" type) (gdax-process-exec (gdax-to-exec json)))
                           ;; ((equal "match" type) (gdax-process-exec (gdax-to-exec json)))
                           ((equal "heartbeat" type) (message "Heartbeat"))
                           ;; (t (message "type %s" type))
                           ))
                   ;; (message "ws frame: %S" (websocket-frame-text frame))
		   )
     :on-close (lambda (_websocket)
                 (message "Websocket closed")))))

(defun gdax-disconnect ()
  "Disconnect from gdax."
  (interactive)
  (if gdax-websocket
      (progn
        (websocket-close gdax-websocket)
        (setq gdax-websocket nil))
    (message "Not connected!")))

(provide 'btc)

;;
;; Tests
;;

;; (setq test-json "{\"type\": \"snapshot\",\"product_id\": \"BTC-EUR\",\"bids\": [[\"1\", \"2\"]],\"asks\": [[\"2\", \"3\"]]}")

;; (let ((json-array-type 'list))
;;   (rest (assoc 'bids (json-read-from-string test-json))))


;; (setq test-json "{\"type\":\"done\",\"side\":\"sell\",\"order_id\":\"558b6216-e3c0-4385-aa17-7b181a0289d4\",\"reason\":\"canceled\",\"product_id\":\"BTC-USD\",\"price\":\"8951.59000000\",\"remaining_size\":\"0.02000000\",\"sequence\":4421152632,\"time\":\"2017-11-26T04:16:25.440000Z\"}")

;; (string-to-number (cdr (assoc 'price (json-read-from-string test-json))))

;; (let* ((json (json-read-from-string test-json))
;;        (type (cdr (assoc 'type json))))
;;   (if (equal "done" type)
;;       (message "%s"
;;                (string-to-number (cdr (assoc 'price json))))))

;;; btc.el ends here
