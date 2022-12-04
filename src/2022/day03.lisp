(defpackage :aoc/2022/03 #.cl-user::*aoc-use*)
(in-package :aoc/2022/03)

(defun backpacks ()
  (mapcar [coerce _ 'list]
          (uiop:read-file-lines #P"src/2022/day03.txt")))

(defun item-priority (c)
  (1+ (position c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(defun find-duplicate-item (&rest lists)
  (first (reduce #'intersection (cdr lists) :initial-value (car lists))))

(defun solution-run ()
  (values
    (loop for pack in (backpacks)
          for (c1 c2) = (split pack (/ (length pack) 2))
          sum (item-priority (find-duplicate-item c1 c2)))
    (loop for (b1 b2 b3) on (backpacks) by #'cdddr
          sum (item-priority (find-duplicate-item b1 b2 b3)))))

(define-test (2022 03) (7746 2604))

;;; Version 1 (this):
;;; - Centralize calls to COERCE
;;; - Get rid of all the CHAR-CODE calls and replace them with a call to POSITION against an _alphabet_ string
;;; - Usual plumbing
;;; - [link](https://topaz.github.io/paste/#XQAAAQAqAwAAAAAAAAAUGQimgx+p6Nl0Rx3xJ86tXw/1Bkm6TwhC62+umXpy1poQYmhpp8w/+IomqBMFIpYgvwCh0OczYLCDGbBsmHXmUchepr7+Xuyxn/dZXX9hS6eHIg/4+A45JZdW1WO+fZbRHe1Sco78NGdkZEsPcz0WDPc4HJtptzpt+bUl1/Nb2RHehXUT66oi0AMduEGlY9qFwEWqykmoYz9qUJ1kjynVRMpkd1NHNKUWYByfSj4tqdQebVZ53HUhkFAjl/kfwvw+ZcYU/hIwjXYYT1s7qs/kPpIdhDN1ji6Ea/R88rbQcL5IBtFgpxSCzUFKFBcqct4E1dvJAKopgq1AvUu59Slx60AXofPJL9T7Esr9ROpGC0MS5Wl2vO9ZYoR82fyforbtseSX1yDtsxvYnV22Zx+rdP4nr81Rq5lq9Mx3x+hIkok3huY49TOjCkAuJlNMyS5vAjVm/1GenLy1+qhkkgMf/FAGbkoORGK+bVp4D4qAZckqSkYK1kTMKHgyWHv/02EgiZ75/DrmWu3kjue/Lmz7u0CxgBSYHLBckiH/QdYOZxoA/5kB32+rZ0+avaFOiktcu8BtlZcJh2VFpq7mU8b/7FDztg==)
;;; Version 0:
;;; - Lots of copy-pasta
;;; - CHAR-CODE calls all over the place
;;; - [link](https://topaz.github.io/paste/#XQAAAQA2BAAAAAAAAAAd6AQKBhOR2ycIlwk9XHxL32WTec9dUvX+8eemGBsEyESBxs9VTt13UYH4A7UlkBJU1CnCgSpwhZhMNdc8wxkH6ZewpOxpSSqGipnlo3RS16Um+EYQ47tHpXyEahytPJN5Y7/cQeDTsNl5uLk6am9oYrm40A0CpoQzVfNn3P/zVlBL86/b2wV/Zaod0gbfKeYwqs4apa+5bW5t3rd4eueqJrI0hCYs4nsW0xwEH+5WI636gnjtThJTKQy5TyNK+qtxVVWafuIwSRJ8kKYg9/5BriW5hsjwW3rY2MtImAwIdKV160nVsnPuQkwhV5L1MrN2ag/LJW1EJm8srVilhvSMQH4z35UhKu3VTaySew7QREYbiXgmbfsKt2IjKIJ5fKo8AJuPVtPIM5L/Vjb+UgMHU03kRP6uUe+M13jEa8MY4bHQ+jHKu6TgRTX1if1sWX8=)
