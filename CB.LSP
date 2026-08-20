;;; --------------------------------------------------------------------
;;; CB.LSP - Outil de copie multiple de bloc avec orientation dynamique
;;; Auteur       : Vibe Code (pour pqliste-bot/LISP)
;;; Date         : 2024
;;; Description  : Sélectionnez un bloc, puis copiez-le plusieurs fois.
;;;                Pour chaque copie : 1. Point d'insertion, 2. Direction.
;;;                Appuyez sur ÉCHAP pour quitter.
;;; Utilisation  : Tapez **CB** dans AutoCAD.
;;; --------------------------------------------------------------------

(defun c:CB (/ *error* doc ss ent entData blkName newPt dirPt dx dy angle)

  ;; --- Chargement des fonctions Visual LISP ---
  (vl-load-com)

  ;; --- Récupération du document AutoCAD ---
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  ;; --- Gestionnaire d'erreurs ---
  (defun *error* (msg)
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

  ;; --- Message d'instruction ---
  (princ "\n✅ Block selected. Now: 1. Click insertion point, 2. Click direction. Press ESC to exit.")

  ;; --- Boucle pour copier plusieurs fois ---
  (while T
    ;; --- Point d'insertion ---
    (setq newPt (getpoint "\n📍 Insertion point: "))
    (if (not newPt) (break))  ;; Sortie si ÉCHAP

    ;; --- Direction pour l'orientation ---
    (setq dirPt (getpoint newPt "\n🎯 Click in the desired direction: "))
    (if (not dirPt) (break))  ;; Sortie si ÉCHAP

    ;; --- Calcul de l'angle ---
    (setq dx (- (car dirPt) (car newPt)))
    (setq dy (- (cadr dirPt) (cadr newPt)))
    (setq angle (/ (* (atan dy dx) 180.0) pi))

    ;; --- Insertion du bloc ---
    (command "_.INSERT" blkName newPt "" "" angle)

    ;; --- Message de confirmation ---
    (princ "\n✅ Block copied! Continue or press ESC to exit.")
  )

  ;; --- Fin de la transaction Undo ---
  (if doc (vla-endundomark doc))

  ;; --- Message de fin ---
  (princ "\n🛑 Multi-copy mode exited.")
  (princ)
)
