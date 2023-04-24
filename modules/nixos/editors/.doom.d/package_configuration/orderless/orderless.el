;;; package_configuration/orderless.el -*- lexical-binding: t; -*-

(after! orderless
	(setq completion-styles '(orderless))
	(setq completion-category-defaults nil)
	(setq orderless-component-separator 'orderless-escapable-split-on-space))
