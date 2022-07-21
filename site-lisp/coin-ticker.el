;;; coin-ticker.el --- Display coin prices on mode-line

;; 参考 https://github.com/eklitzke/coin-ticker-mode

;;; Require:
(require 'request)

;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Group ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup coin-ticker nil
  "coin-ticker extension"
  :group 'comms
  :prefix "coin-ticker-")

(defcustom coin-ticker-count 0
  "持有数量"
  :group 'coin-ticker)

(defcustom coin-ticker-usd-to-cny 6.43
  "美元与人民币的汇率"
  :type 'number
  :group 'coin-ticker)

(defcustom coin-ticker-url "https://coinmarketcap.com/currencies/shiba-inu/"
  "默认为 shiba"
  :group 'coin-ticker)

(defcustom coin-ticker-price-format 100000
  "显示币价时乘以的倍数"
  :type 'number
  :group 'coin-ticker)

(defcustom coin-ticker-update-interval 10
  "更新间隔"
  :type 'number
  :group 'coin-ticker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar coin-ticker-modeline nil
  "mode-line 显示的内容")

(defvar coin-ticker-timer nil
  "定时器")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun coin-ticker-modeline-update (price)
  (let ((reformatted-price (* price coin-ticker-price-format))
        (asset (* price coin-ticker-count coin-ticker-usd-to-cny)))
    (setq coin-ticker-modeline (format " %.3f ￥%.1f " reformatted-price asset))))

(defun coin-ticker-price-update ()
  (request
    coin-ticker-url
    :params 'nil
    :parser 'buffer-string
    :success
    (cl-function (lambda (&key data &allow-other-keys)
                   (when data
                     (let ((price
                            (progn (string-match "<div.*?priceValue.*?>$\\(.*?\\)<\\/div>" data)
                                   (match-string 1 data))))
                       (coin-ticker-modeline-update (string-to-number price))))))))

(defun coin-ticker-start ()
  (add-to-list 'mode-line-misc-info '((:eval coin-ticker-modeline)))
  (unless coin-ticker-timer
    (setq coin-ticker-timer
          (run-at-time "0 sec" coin-ticker-update-interval #'coin-ticker-price-update))
    (coin-ticker-price-update)))

(defun coin-ticker-stop ()
  (when coin-ticker-timer
    (cancel-timer coin-ticker-timer))
  (setq mode-line-misc-info (delete '((:eval coin-ticker-modeline)) mode-line-misc-info))
  (setq coin-ticker-modeline nil)
  (setq coin-ticker-timer nil))

(define-minor-mode coin-ticker-mode
  "Minor mode to show cryptocurrency prices."
  :init-value nil
  :global t
  (if coin-ticker-mode
      (coin-ticker-start)
    (coin-ticker-stop)))

(provide 'coin-ticker)

;;; coin-ticker.el ends here
