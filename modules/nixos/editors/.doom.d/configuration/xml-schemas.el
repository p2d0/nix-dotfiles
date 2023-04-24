;;; editors/.doom.d/configuration/xml_schemas.el -*- lexical-binding: t; -*-

(after! rng-loc
	(add-to-list 'rng-schema-locating-files "~/.doom.d/schemas/schemas.xml"))
