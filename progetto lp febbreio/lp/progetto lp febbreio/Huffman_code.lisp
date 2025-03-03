(defun sorted-descending-p (list key-fn)
  "Verifica se la LIST è ordinata in ordine decrescente secondo la funzione KEY-FN."
  (every (lambda (a b) (>= (funcall key-fn a) (funcall key-fn b)))
         list
         (rest list)))


(defun tree-weight (tree)
  "Restituisce il peso dell'albero, che si tratti di una foglia o di un nodo interno."
  (if (eq (first tree) 'leaf)
      (third tree)  
      (fourth tree)))  

(defun combine-trees (t1 t2)
  "Combina due alberi in un nuovo nodo, il cui peso è la somma dei pesi dei due alberi."
  (list 'node t1 t2 (+ (tree-weight t1) (tree-weight t2))))


(defun hucodec-generate-huffman-tree (symbols-n-weights)
  "Genera un albero di Huffman a partire da una lista di coppie (simbolo . peso).
Se la lista non è ordinata in ordine crescente in base al peso, la ordina prima."
  (when (null symbols-n-weights)
    (error "La lista di simboli e pesi è vuota"))

  ;; Assicuriamoci che la lista sia ordinata per peso crescente
  (setf symbols-n-weights (sort symbols-n-weights #'< :key #'cdr))

  ;; Creiamo una lista di foglie
  (let ((forest (mapcar (lambda (pair) (list 'leaf (car pair) (cdr pair)))
                        symbols-n-weights)))
    (loop while (> (length forest) 1) do
          (let* ((t1 (pop forest)) ;; Rimuoviamo il primo (minore)
                 (t2 (pop forest)) ;; Rimuoviamo il secondo (minore successivo)
                 (new-tree (combine-trees t1 t2)))
            ;; Inseriamo il nuovo nodo e riordiniamo in ordine crescente
            (setf forest (sort (cons new-tree forest) #'< :key #'tree-weight))))
    (car forest))) ;; L'ultimo elemento rimasto è l'albero completo



(defun hucodec-generate-symbol-bits-table (huffman-tree &optional (prefix "") (table '()))
  "Genera una tabella di codifica dei simboli a partire dall'albero di Huffman.
Restituisce una lista di coppie (simbolo . codice-binario)."
  (if (eq (first huffman-tree) 'leaf)
      (cons (cons (second huffman-tree) prefix) table)
      (nconc (hucodec-generate-symbol-bits-table (second huffman-tree) (concatenate 'string prefix "0") table)
             (hucodec-generate-symbol-bits-table (third huffman-tree) (concatenate 'string prefix "1") table))))

(defun hucodec-encode (message huffman-tree)
  "Codifica MESSAGE in una stringa di bit usando l'albero di Huffman."
  (let* ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree))
         (ht (make-hash-table :test #'eql)))
    ;; Creiamo una hash table per una ricerca veloce dei codici
    (dolist (pair symbol-bits-table)
      (setf (gethash (car pair) ht) (cdr pair)))
    ;; Convertiamo ogni carattere del messaggio nei bit corrispondenti
    (apply #'concatenate 'string
           (mapcar (lambda (char) (gethash char ht ""))
                   (if (stringp message) (coerce message 'list) message)))))


(defun hucodec-encode-file (filename huffman-tree)
  "Codifica il contenuto di un file usando la codifica di Huffman e lo salva in un nuovo file."
  (with-open-file (in filename :direction :input)
    (let* ((content (make-string (file-length in)))
           (encoded-bits (hucodec-encode (read-sequence content in) huffman-tree))
           (encoded-filename (concatenate 'string filename ".huff")))
      (with-open-file (out encoded-filename :direction :output :if-exists :supersede)
        (write-string encoded-bits out))
      encoded-filename)))

(defun hucodec-decode (bits huffman-tree)
  "Decodifica una stringa di bit usando l'albero di Huffman e restituisce il messaggio originale."
  (let ((decoded-message '())
        (current-tree huffman-tree))
    (loop for bit across bits do
      (setf current-tree (if (char= bit #\0)
                             (second current-tree) ;; ramo sinistro
                             (third current-tree))) ;; ramo destro
      (when (eq (first current-tree) 'leaf)
        (push (second current-tree) decoded-message)
        (setf current-tree huffman-tree)))
    (coerce (nreverse decoded-message) 'string)))


(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  "Stampa l'albero di Huffman in modo leggibile con indentazione."
  (let ((indent (make-string (* 2 indent-level) :initial-element #\space)))
    (if (eq (first huffman-tree) 'leaf)
        (format t "~aLeaf: ~a (~a)~%" indent (second huffman-tree) (third huffman-tree))
        (progn
          (format t "~aNode (~a)~%" indent (fourth huffman-tree))
          (hucodec-print-huffman-tree (second huffman-tree) (+ indent-level 1))
          (hucodec-print-huffman-tree (third huffman-tree) (+ indent-level 1))))))
