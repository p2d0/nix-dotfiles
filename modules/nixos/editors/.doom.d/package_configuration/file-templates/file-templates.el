(defun +file-template-apply ()
  "apply file-template to current buffer from file-templates list"
  (when-let (rule (cl-find-if #'+file-template-p +file-templates-alist))
    (apply #'+file-templates--expand rule)))


(defun open-buffer-and-insert-template (path)
  (when (not (f-dir? path))
    (select-window (next-window (selected-window)))
    (find-file path)
    (+file-template-apply)))

(add-hook 'treemacs-create-file-functions #'open-buffer-and-insert-template t)
(add-hook 'neo-create-file-hook #'open-buffer-and-insert-template t)

(set-file-templates!
  '("\\(test\\|spec\\)\\.py$"   :trigger "__test.py"    :mode python-mode)
  '("[sS]pec\\.js$" :trigger "__spec.js" :mode js2-mode)
  '("\\.cs$" :trigger "__.cs" :mode csharp-mode)
  '("Command\\.cs$" :trigger "__Command.cs" :mode csharp-mode)
  '("InternalRequestHandler\\.cs$" :trigger "__InternalRequestHandler.cs" :mode csharp-mode)
  '("InternalRequest\\.cs$" :trigger "__InternalRequest.cs" :mode csharp-mode)
  '("InternalRequestResponse\\.cs$" :trigger "__InternalRequestResponse.cs" :mode csharp-mode)
  '("Repository\\.cs$" :trigger "__Repository.cs" :mode csharp-mode)
  '("Controller\\.cs$" :trigger "__Controller.cs" :mode csharp-mode)
  '("Enum\\.cs$" :trigger "__Enum.cs" :mode csharp-mode)
  '("validator\\.ts$" :trigger "validator.ts" :mode typescript-mode)
  '("store\\.ts$" :trigger "__store.ts" :mode typescript-mode)
  '("prompt\\.js$" :trigger "__prompt.js" :mode rjsx-mode)
  '("Form\\.tsx$" :trigger "__Form.tsx" :mode typescript-tsx-mode)
  '("CommandHandler\\.cs$" :trigger "__CommandHandler.cs" :mode csharp-mode)
  '("CommandResult\\.cs$" :trigger "__CommandResult.cs" :mode csharp-mode)
  '("\\.service$" :trigger "systemd_service" :mode fundamental-mode)
  '("EntityTypeConfiguration\\.cs$" :trigger "__EntityTypeConfiguration.cs" :mode csharp-mode)
  '("Test\\.cs$" :trigger "__Test.cs" :mode csharp-mode)
  )

(add-hook 'python-mode-hook
  '(lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
