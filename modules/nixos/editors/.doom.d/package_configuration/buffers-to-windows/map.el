;;; package_configuration/buffers-to-windows/map.el -*- lexical-binding: t; -*-


(dotimes (i 9)
  (let ((n (+ i 1)))
    (let ((key (format "b%i" n))
          (func (intern (format "buffer-to-window-%s" n))))
      (map!
       (:leader key func)))))
