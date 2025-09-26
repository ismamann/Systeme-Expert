


(setq *baseDeRegles2* '( 
                        (P1 ((> TT 9) R1))
                        (P3 ((> TT 9) R2))
                        (RT ((> TT 9) R3))
                        (P0 ((et (> TT 2) (< TT 9)) R4))
                        (P1 ((et (> TT 2) (< TT 9)) R5))
                        (P0 ((et (> TT 0) (< TT 3)) R6))
                        (RT-bar ((et (> TT 0) (< TT 3)) R7))
                        (RT ((et (> TT 2) (< TT 9)) R8))
                        (7A ((Re0) R9))
                        (P3 ((T0) R10))
                        (GM ((A-bar) R11))
                        (Re0 ((et (et (> TT 2) (< TT 9)) A) R12))
                        (Re1 ((et (> TT 2) (< TT 9)) R13))
                        (Re2 ((et (> TT 2) (< TT 9)) R14))
                        (GM ((P3) R15))
                        (P0-bar ((P3) R16))
                        (P3-bar ((ou P0 P1) R17))
                        (TP ((ou Re0 (et (et (> TT 2) (< TT 9)) A)) R18))
                        (RE ((et Re1 Re2) R19))
                        (TP ((et RE (et (> TT 2) (< TT 9))) R20))
                        (M ((et GM A-bar Re1 (ou P0 P1)) R21))
                        (7c ((ou M B) R22))
                        (7b ((ou M B) R23))
                        (Ry ((et Re0 A (ou P0 P1)) R24))
                        (7A ((Ry) R25))
                        (7B ((Ry) R26))
                        (7D ((Ry) R27))
                        (B ((et GM TP P3 Re2) R28))
                        (TI ((et A (et (> TT 0) (< TT 3))) R29))
                        (Re0-bar ((A-bar) R30))
                        (7B ((A-bar) R31))
                        (Re0 ((A) R32))
                        (J ((et TI Re0-bar) R33))
                        (7AS ((et IS 7A) R34))
                        (7BS ((et IS 7B) R35))
                        (7CS ((et IS 7C) R36))
                        (7DS ((et IS 7D) R37))))




(setq *baseDefaits* '(b c))
(defun cclRegle (regle) (car regle))
(defun premisseRegle (regle) (cadr regle))
(defun numRegle (regle) (caddr regle))



(defun verifCond(bdf regle condition)
  (cond ((EQ condition 'ET)
         (dolist (x regle nil)
           (if (EQ nil (member x bdf))
               (return-from verifCond nil)))
         (return-from verifCond T))
        ((EQ condition 'ou)
         (dolist (x regle nil)
           (if (member x bdf)
               (return-from verifCond T))))
        (T
         (return-from verifCond (funcall condition (eval (car regle)) (cadr regle)))))
  nil)



(defun regles_candidates2(bdf bdr)
  (let ((res nil) (bool 0) (op 'et) (premisse nil))
    (dolist (x bdr res) 
      (cond ((not (EQ 1 (length (caadr x)))) 
             (setq op (caaadr x))
             (setq premisse (cdaadr x)))
            ((EQ 1 (length (caadr x)))
             (setq premisse (caadr x))))
        
      (if (EQ op 'et)
          (setq bool 1)
        (setq bool 0))
      (cond ((EQ nil (member (car x) bdf))
             (dolist (y premisse nil)
               (if (listp y)
                   (cond ((and (EQ 'et op) (EQ nil (verifCond bdf (cdr y) (car y))))
                          (setq bool 0))
                         ((and (EQ 'ou op) (verifCond bdf (cdr y) (car y)))
                          (setq bool 1)))
                          
                          
                 (cond ((and (EQ 1 bool) (EQ 'et op) (EQ nil (member y bdf)))
                        (setq bool 0))
                       ((and (EQ 0 bool) (EQ 'ou op) (member y bdf))
                        (setq bool 1)))))
             (if (EQ 1 bool)
                 (pushnew (car x) res)))))
    res))
            
     



(defun trouveQuestion(bdf bdr bdq)
  (dolist (x bdr nil)
     (cond ((and (assoc (car x) bdq) (EQ nil (member (car x) bdf)) (EQ nil (member (car x) pasDeQuestion)))
           (let ((answer nil))
             (format t "~a  Y (oui) ou N (non) : ~&" (cadr (assoc (car x) bdq)))
             (loop
               while (and (not (equal answer "Y")) (not (equal answer "N"))) do
               (print "Seulement Y ou N : ")
               (setq answer (read-line)))
             (pushnew (car x) pasDeQuestion) 

             (if (equal answer "Y")
                 (return-from trouveQuestion (car x)))))))
  nil)
        
      

(defun butTrouve(but bdf)
  (dolist (x but nil) 
    (dolist (y bdf nil)
      (if (EQ x y)
          (return-from butTrouve T))))
  nil)




(defun moteurInferenceLarg2(but bdf bdr bdq limite)
  (if (member but bdf)
      (return-from moteurInferenceLarg2 bdf))
  (let ((candidates nil) (cpt 0) (tmp nil) (res (list bdf)))
    (loop
      (setq candidates (regles_candidates2 (car res) bdr))
      (when (butTrouve but (car res)) (return-from moteurInferenceLarg2 (reverse (car res))))
      (dolist (x candidates nil)
        (setq tmp (car res))
        (pushnew x tmp)
        (setq res (append res (list tmp))))
      (cond ((and (EQ 1 (length res)) (<= cpt limite))
             (incf cpt)
              (pushnew (trouveQuestion bdf bdr bdq) (car res)))
             ((EQ 1 (length res)) (EQ cpt (+ 1 limite))
              (return-from moteurInferenceLarg2 (car res)))
             ((>= (length res) 1)
              (pop res))))))



(defun main ()

	 (printExplanation)
         (setq pasDeQuestion '(RE IS M B Ry J TP 7AS 7BS 7D 7DS ))

         (setq *BF* NIL)

	(print "Choix 1 : profil prefrabrique. / Choix 2 : profil personnalise. 1 ou 2 ? : ")
	(let ((answer (read-line)))

		; Check reponse correcte
		(loop while (and (not (equal answer "1")) (not (equal answer "2"))) do
				(print "Seulement 1 ou 2 : ")
				(setq answer (read-line))
			)

		(if (equal answer "2")
			(progn
				(print "Lancement de la fabrication de BF")
				(init)
			)
			(progn
				 (print "Preparation de la BF automatiquement")
                                (setq TT 8)
				(setq *BF* '(IS TT GM A-bar Re2 P3))
			)
		)
	)

	(print "Voici la base de faits actuelle : ")
	(print *BF*)

	 ;(beginExploration)
  
 (setq *BF* (moteurInferenceLarg2 '(M B Ry J) *BF* *baseDeRegles2* *BDQ* 1)) 
  (print *BF*)
	(showFinished)
	NIL

)

(defun printExplanation () 
  (print "Bienvenue dans le travail en profondeur grace au Systeme Expert !")
  (print "Laissez-vous guider et decouvrez le profil ainsi que les conseils qui vous conviennent."))

(defun init()
  (print "Vous ne connaissez pas l’importance des indicateurs stratégique ? Y (oui) ou N (non) : ")
  (setq answer (read-line))
  
  ; Check reponse correcte
  (loop while (and (not (equal answer "Y")) (not (equal answer "N"))) do
				(print "Seulement Y ou N : ")
				(setq answer (read-line))
			)

		(if (equal answer "Y")
			(pushnew 'IS *BF*)
		  )

  
  (print "Arrivez-vous à faire abstraction de l'alimentation, du sommeil,") 
  (print "du sexe ou des usages d'internet lorsque vous entrepenez un tache ? Y (oui) ou N (non) : ")
  (setq answer (read-line))
  
  ; Check reponse correcte
  (loop while (and (not (equal answer "Y")) (not (equal answer "N"))) do
				(print "Seulement Y ou N : ")
				(setq answer (read-line))
			)

		(if (equal answer "Y")
			(pushnew 'A-bar *BF*)
			(pushnew 'A *BF*)
		  )
  
  (print "Combien de temps souhaitez-vous accorder a la realisation de votre objectif ? ")
  (print "Sur une echelle de 0 à 10, choisissez le nombre d'heures : ")
  ; Réponse de l'utilisateur
  (setq TT (parse-integer (read-line)))
  
  ; Check reponse correcte
  (loop while (and (not (> TT 0)) (not (< TT 10))) do
        (print "Seulement entre 0 et 10 heures: ")
        (setq TT (parse-integer (read-line)))
        )
  ; Push dans la BF
  (pushnew 'TT *BF*)
  )

(defun showFinished ()
  (let (
        (index 1)
        (adviseOrder NIL)
	(adviseChosen NIL)
	(subAdvise NIL)
        (BFAdvise NIL)
        )
    (dolist (x *BF* nil)
      (if (assoc x listAdvise)
          (pushnew (car (assoc x listAdvise)) BFAdvise)))
    
    (if BFAdvise
        (progn
          (print "Choisissez le conseil a afficher grace au numero!")
          (print "0: Pour sortir.")
          (dolist (currentAdvise BFAdvise)
            (if currentAdvise
                (progn
                  
           (print index)
           (princ ": ")
           (princ currentAdvise)
           (push (list index currentAdvise) adviseOrder)
           (incf index)
           )
       ()
       )
     )
          (print "Entrez le numero du conseil a afficher: ")
          (setq adviseChosen (parse-integer (read-line)))
          (setq adviseChosen (cadr (assoc adviseChosen adviseOrder)))
          (print (cadr (assoc adviseChosen listAdvise)))
          ; Check reponse correcte
          (if (not (eq adviseChosen NIL))
                   (showFinished)
                   (return-from showFinished "A bientot !")
           )
          )
      (print "Le SE n'a pas trouve de conseil !")
      )
    )
)

(setq listAdvise '(
                   (M "Vous pouvez opter pour une philosophie monastique. 
Vous etes un moine ! 
Cette philosophie tente d'optimiser les efforts pour atteindre la profondeur en eliminant ou diminuant considerablement les obligations superficielles. 
Autrement dit, l'equation de la productivite n'est pas lineaire chez vous. 
C'est en organisant votre vie de facon a disposer de tres long creneaux successifs sans interruption que vous serez en mesure d'exceller dans vos projets.")                  
                   (B "Vous pouvez opter pour une philosophie bimodale. 
Consacrez une partie de votre temps a des activites profondes et laissez le restant ouvert a n'importe quelle autre tache. 
Cette division du temps entres les taches profondes et la liberte d'action peut s'inscrire sur differentes durees (jours,semaine,saison). 
Vous pouvez atteindre une productivite extreme mais seulement si vous consacrez au moins une journee entiere a un travail en profondeur.")
                   (Ry "Vous pouvez opter pour une philosophie rythmique. 
Pour reussir vos seances de travail, il faut les transformer en simple habitude.
Autrement dit, le but est de generer un rythme eliminant le besoin de depenser de l'energie a decider si et quand vous allez vous engager dans un travail.")
                   (J  "Vous pouvez opter pour une philosophie journalistique. 
Vous avez besoin de travailler quasi aleatoirement. 
Casez des que possible dans votre emploi du temps un seance de travail. 
Soyez vigilant a la bascule entre les moments 'superficiels' et le travail en profondeur, en effet votre reservoir de volonte peut-etre affecte. 
Une certaine confiance en vos capacites; avec la conviction que ce que vous faites est important et portera ces fruits, vous aidera a reussir.")
                   (GM "Voici un concept qui peut vous aider : la modification radicale de votre environnement habituel, 
associee a un investissement significatif pour ce qui a trait au temps ou a l'argent, 
tout cela afin de favoriser l'execution en profondeur d'une tache et l'importance accordee a ladite tache. 
Cette importance percue plus flagrante reduit la tendance a procrastiner et renforce la motivation et l'energie.")
                   
                   (7AS "Plus vous essayez d'en faire, moins vous reussissez de choses. 
L'execution doit concerner un petit nombre d'objectifs primordiaux. Concentrez-vous sur l'essentiel.")
                   
                   
                   (7BS "Vous vous concentrez trop sur les indicateurs de resultat (ce que vous essayez d'ameliorer), 
a la place, penchez-vous sur les indicateurs strategiques (les nouveaux comportements qui meneront au succes).")
                  
                   (7CS "Trouvez un tableau d'affichage stimulant. 
Les gens jouent differemment quand ils comptent les points.")
                   
                   (7DS "Creez-vous un cadence de responsabilisation, des reunions frequentes. 
Trouvez une discipline qui vous convient pour que l'execution prenne vraiment forme.")
                   
                   (Re0 "Si vous voulez etre plus productifs, deconnectez-vous des reseaux sociaux.
Allouez des plages horaires dediees a vos taches sans distraction en ligne. 
En maitrisant votre attention, vous libererez un potentiel creatif et productif insoupconne")
                   (TI "Vos sessions de travail peuvent parfois etre irregulieres. 
Il est donc necessaire d'identifier vos periodes productives et de vous accorder des pauses regulieres. 
L'organisation demeure essentielle pour maximiser l'efficacite")
                   (TP "Les genies meme ne furent grands que par l'application de toute leur force sur le point ou 
ils avaient decide de donner leur mesure. Vous avez le potentiel de travailler en profondeur, utilisez-le !")
                   ))

(setq *BDQ* '((Re0 "Pensez-vous que vous devez vous couper des reseaux pour une tache particuliere ? ")
            (Re0-bar "Pensez-vous que vous ne devez pas vous couper des reseaux pour une tache particuliere ? ")
            (Re1 "Pensez-vous que votre dernier mois serait mieux sans réseaux sociaux ?")
            (Re2 "Pensez-vous que cela ne ferait rien à vos proches si vous arretez les reseaux?")
            (IS "Vous ne connaissez pas l’importance des indicateurs stratégique ?")
            (P0 "Pour vous changer les idees, ferez-vous une pause cafe ?")
            (P1 "Pour vous changer les idees, ferez-vous une balade en foret ?")
            (P2 "Pour vous changer les idees, ferez-vous un weekend ou plus loin de chez vous ?")
            (GM "Etes-vous bloque dans votre travail ?")
            (RT "Prenez-vous de long temps de reflexion avant de demarrer un projet ?")
           
              ))
(main)



  