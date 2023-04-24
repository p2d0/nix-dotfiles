;;; snippet-helper-functions/csharp-mode/constructor.el -*- lexical-binding: t; -*-

(defun +yas-csharp/without_ (text)
  (replace-regexp-in-string "_" "" text))

(defun +yas-csharp/get_last_two_words (text)
  (let* ((arr (split-string text)))
    (if (>= (length arr) 2)
      (s-join " " (seq-subseq arr -2))
      (s-join " " arr))))

(defun +yas-csharp/get_last_word (text)
  (let* ((arr (split-string text " ")))
    (car (last arr))))

(defun +yas-charp/add-newlines (text)
  (->> (split-string text ";")
    (s-join ";\n\t\t")))

(defun +yas-csharp/->constructorArguments (text)
  (let* ((arr (split-string text ";\n")))
    (->> (mapcar (lambda (item) (+yas-csharp/get_last_two_words item)) arr)
      (s-join ",")
      (+yas-csharp/without_)
      (replace-regexp-in-string ";" ""))))

(defun +yas-csharp/->assignment (text)
  (let* ((arr (split-string text ";\n")))
    (->> (mapcar
	   (lambda (item) (let* ((last_word (replace-regexp-in-string ";" "" (+yas-csharp/get_last_word item))))
		     (s-concat  last_word " = " (+yas-csharp/without_ last_word))))
	   arr) (s-join ";\n"))))
