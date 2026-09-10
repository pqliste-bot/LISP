;;; --------------------------------------------------------------------
;;; CB.LSP - Outil de copie multiple de bloc avec orientation dynamique
;;; Auteur       : Vibe Code (pour pqliste-bot/LISP)
;;; Date         : 2024
;;; Description  : Sélectionnez un bloc, puis copiez-le plusieurs fois.
;;;                Pour chaque copie : 1. Point d'insertion, 2. Direction.
;;;                Pendant la sélection de la direction, un aperçu
;;;                transparent du bloc suit le curseur pour visualiser
;;;                sa position et son orientation finales avant validation.
;;;                La boucle se répète après chaque copie pour enchaîner
;;;                plusieurs insertions/orientations d'affilée.
;;;                Appuyez sur ÉCHAP à tout moment pour quitter.
;;; Utilisation  : Tapez **CB** dans AutoCAD.
;;; --------------------------------------------------------------------

(defun c:CB (/ *error* doc ss ent entData blkName sourceObj
               newPt gr gtype gval dx dy angRad ghost run preview)

  ;; --- Chargement des fonctions Visual LISP ---
  (vl-load-com)

  ;; --- Récupération du document AutoCAD ---
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  ;; --- Gestionnaire d'erreurs ---
  ;; Nettoie l'éventuel bloc fantôme en cours de prévisualisation.
  (defun *error* (msg)
    (if (and ghost (not (vlax-erased-p ghost)))
      (vl-catch-all-apply 'vla-delete (list ghost))
    )
    (if (not (wcmatch (strcase msg) "*BREAK* *CANCEL* *EXIT*"))
      (princ (strcat "\n❌ Error: " msg))
    )
    (if doc (vla-endundomark doc))
    (princ)
  )

  ;; --- Début de la transaction Undo (pour annuler tout avec CTRL+Z) ---
  (if doc (vla-startundomark doc))

  ;; --- Étape 1 : Sélection du bloc source (1 seule fois) ---
  (setq ss (entsel "\n🔹 Select the block to copy: "))
  (if (not ss)
    (progn
      (if doc (vla-endundomark doc))
      (princ "\n⚠️ No block selected. Command canceled.")
      (exit)
    )
  )

  ;; --- Vérification que c'est bien un bloc ---
  (setq ent (car ss))
  (setq entData (entget ent))
  (setq blkName (cdr (assoc 2 entData)))

  (if (not blkName)
    (progn
      (if doc (vla-endundomark doc))
      (alert "❌ The selected entity is not a block.\nPlease select a valid block.")
      (exit)
    )
  )

  ;; --- Objet VLISP du bloc source (pour copie + transparence) ---
  (setq sourceObj (vlax-ename->vla-object ent))

  ;; --- Message d'instruction ---
  (princ "\n✅ Block selected. Repeat: 1. Insertion point, 2. Direction. Press ESC to exit.")

  ;; --- Boucle principale : se répète après chaque copie ---
  (setq run T)
  (while run
    ;; --- Point d'insertion ---
    (setq newPt (getpoint "\n📍 Insertion point (ESC to exit): "))
    (if (not newPt)
      (setq run nil)  ;; ÉCHAP -> arrêt de la commande
    )

    (if run
      (progn
        ;; --- Création du bloc fantôme (aperçu) ---
        (setq ghost (vla-Copy sourceObj))
        (vla-put-InsertionPoint ghost (vlax-3d-point newPt))
        (vla-put-Rotation ghost 0.0)
        ;; Transparence 80% pour bien distinguer l'aperçu du bloc final
        (vl-catch-all-apply 'vla-put-EntityTransparency (list ghost 80))
        (vla-Update ghost)

        (princ "\n🎯 Move cursor to set direction, click to confirm (ESC to exit).")

        ;; --- Boucle de prévisualisation dynamique (orientation) ---
        (setq preview T)
        (setq angRad 0.0)
        (while preview
          (setq gr (grread T 1 1))
          (setq gtype (car gr))
          (setq gval  (cadr gr))

          (cond
            ;; --- Déplacement de la souris : mise à jour de l'orientation ---
            ((= gtype 5)
              (setq dx (- (car gval) (car newPt)))
              (setq dy (- (cadr gval) (cadr newPt)))
              (setq angRad (atan dy dx))
              (vla-put-Rotation ghost angRad)
              (vla-Update ghost)
            )

            ;; --- Clic gauche : validation de la direction ---
            ((= gtype 3)
              (setq dx (- (car gval) (car newPt)))
              (setq dy (- (cadr gval) (cadr newPt)))
              (if (or (/= dx 0.0) (/= dy 0.0))
                (setq angRad (atan dy dx))
              )
              ;; Le bloc fantôme devient la copie finale : transparence retirée
              (vl-catch-all-apply 'vla-put-EntityTransparency (list ghost "ByLayer"))
              (vla-Update ghost)
              (setq ghost nil)
              (setq preview nil)
            )

            ;; --- Entrée clavier ---
            ;;     Entrée/Espace : valide avec l'angle courant
            ;;     ÉCHAP (chaîne vide) : annule l'aperçu et arrête la commande
            ((= gtype 2)
              (cond
                ((= gval 13)
                  (vl-catch-all-apply 'vla-put-EntityTransparency (list ghost "ByLayer"))
                  (vla-Update ghost)
                  (setq ghost nil)
                  (setq preview nil)
                )
                ((= gval 32)
                  (vl-catch-all-apply 'vla-put-EntityTransparency (list ghost "ByLayer"))
                  (vla-Update ghost)
                  (setq ghost nil)
                  (setq preview nil)
                )
                ((= gval "")
                  (vl-catch-all-apply 'vla-delete (list ghost))
                  (setq ghost nil)
                  (setq preview nil)
                  (setq run nil)
                )
              )
            )
          )
        )

        ;; --- Message de confirmation (uniquement si on continue) ---
        (if run
          (princ "\n✅ Block copied! Repeat or press ESC to exit.")
        )
      )
    )
  )

  ;; --- Fin de la transaction Undo ---
  (if doc (vla-endundomark doc))

  ;; --- Message de fin ---
  (princ "\n🛑 Multi-copy mode exited.")
  (princ)
)
