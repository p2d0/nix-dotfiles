;;; package_configuration/dap-mode/map.el -*- lexical-binding: t; -*-


(map!
  :leader
  "dd" #'dap-hydra
  "dr" #'dap-breakpoint-delete-all
  "ds" #'dap-debug
	"dp" #'dap-tooltip-at-point
	"db" #'dap-breakpoint-toggle
	"dn" #'dap-next
	"de" #'dap-ui-expressions-add
	"dc" #'dap-continue
	"dQ" #'dap-disconnect
  )

(map!
  (:map '+dap-running-session-mode-map
    :localleader
    "er" #'dap-repl-eval-region
		"p" #'dap-tooltip-at-point
		)
  )
