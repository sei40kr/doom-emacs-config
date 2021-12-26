;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/modules/completion/corfu/packages.el

(package! corfu :pin "61427d8aac4b95fad7a8bbd7b83d9dd4f0b24651")
(package! cape :pin "ba24e62961a341f811a12017c9024be27ae16a37")
(package! kind-icon :pin "7de007bd2be6a207884b2a166ff3bcbb8a96ebc6")
(package! corfu-doc
  :recipe (:host github :repo "galeo/corfu-doc")
  :pin "d05a3caeb8745ed29699baae7970972dd149ff81")
