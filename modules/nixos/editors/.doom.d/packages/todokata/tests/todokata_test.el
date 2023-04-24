;;; packages/todokata/tests/todokata_test.el -*- lexical-binding: t; -*-

(load-file "../todokata.el")

(describe "todokata"
  (after-each
    (when (get-buffer "*todokata*") (kill-buffer "*todokata*") ))
  (it "Should display to-do items"
    (todokata--display '("Item1" "Item2"))
    (with-current-buffer "*todokata*"
      (let ((str (buffer-string)))
	(expect (s-contains? "Item1" str) :to-be-truthy)
	(expect (s-contains? "Item2" str) :to-be-truthy))))
  (it "Should be able to mark to-do items done"
    (todokata--display '("Item1" "Item2"))
    (with-current-buffer "*todokata*"
      (goto-char (- (point-max) 2))
      (todokata--mark-done)
      (todokata--mark-done)
			(expect (thing-at-point 'line) :to-equal "[ ] Item2\n")
      (todokata--mark-done)
      (expect (thing-at-point 'line) :to-equal "[x] Item2\n")))
	(it "Should be able to add to-do items"

		)
	)

