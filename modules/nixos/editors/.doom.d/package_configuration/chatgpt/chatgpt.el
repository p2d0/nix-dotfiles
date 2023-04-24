;;; editors/.doom.d/package_configuration/chatgpt/chatgpt.el -*- lexical-binding: t; -*-

;; (setq chatgpt-repo-path "~/.doom.d/packages/ChatGPT.el/")
(set-popup-rule! "*ChatGPT*"
	:ignore t)

(after! gptel
	(defvar chatgpt-query-format-string-map
		'(("doc" . "Please write the documentation for the following function.\n\n%s")
			 ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
			 ("understand" . "What is the following?\n\n%s")
			 ("refactor" . "Please refactor the following code.\n\n%s")
			 ("improve" . "Please improve the following.\n\n%s")))

	(setq gptel-prompt-string "-> ")
	(setq gptel-default-mode 'markdown-mode)
	(setq gptel--num-messages-to-send nil)

	(defun chatgpt--get-query-type ()
		"Helper function to get the type of query."
		(completing-read "Type of Query: " (cons "custom" (mapcar #'car chatgpt-query-format-string-map))))

	(defun chatgpt--get-query-prompt (query-type query)
		"Helper function to get the query prompt based on the query type."
		(if (equal query-type "custom")
			(format "%s\n\n%s" (read-from-minibuffer "ChatGPT Custom Prompt: ") query)
			(format (cdr (assoc query-type chatgpt-query-format-string-map)) query)))

	(defun chatgpt--send-query-prompt (buf prompt)
		"Helper function to send the query prompt to the given buffer."
		(with-current-buffer buf
			(goto-char (point-max))
			(insert prompt)
			(gptel-send)))

	(defun chatgpt-query ()
		"Function to query ChatGPT and send the result to another buffer."
		(interactive)
		(when-let* ((this (buffer-name))
								 (query-type (chatgpt--get-query-type))
								 (query (buffer-substring (region-beginning) (region-end)))
								 (prompt (chatgpt--get-query-prompt query-type query))
								 (buf (completing-read
												"Send query in buffer: " (mapcar #'buffer-name (buffer-list))
												(lambda (buf) (and (buffer-local-value 'gptel-mode (get-buffer buf))
																	 (not (equal this buf)))))))
			(chatgpt--send-query-prompt buf prompt)
			(pop-to-buffer buf)))



	(map! (:leader "sq" #'chatgpt-query
					"sg" #'gptel)
		(:map gptel-mode-map
			"C-K" #'erase-buffer
			(:n "C-<return>" #'gptel-send)
			(:i "C-<return>" #'gptel-send)))

	(defun gptel--insert-response (response info)
		"Insert RESPONSE from ChatGPT into the gptel buffer.

INFO is a plist containing information relevant to this buffer.
See `gptel--url-get-response' for details."
		(let* ((content-str (plist-get response :content))
						(status-str  (plist-get response :status))
						(gptel-buffer (plist-get info :gptel-buffer))
						(response-pt (plist-get info :insert-marker)))
			(if content-str
        (with-current-buffer gptel-buffer
          (setq content-str (gptel--transform-response
															content-str gptel-buffer))
          (save-excursion
            (put-text-property 0 (length content-str) 'gptel 'response content-str)
            (message "Querying ChatGPT... done.")
            (goto-char response-pt)
            (unless (bobp) (insert-before-markers-and-inherit "\n------------------------\nAnswer:\n"))
            (if gptel-playback
              (gptel--playback gptel-buffer content-str response-pt)
              (let ((p (point)))
                (insert content-str)
                (pulse-momentary-highlight-region p (point)))
              (when gptel-mode
                (insert "\n\n" gptel-prompt-string)
                (gptel--update-header-line " Ready" 'success))))
          (goto-char (- (point) 2)))
				(gptel--update-header-line
					(format " Response Error: %s" status-str) 'error))))
	)
