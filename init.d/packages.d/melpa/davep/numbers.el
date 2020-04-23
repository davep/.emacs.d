(use-package numbers
  :ensure t
  :bind
  ("<f12> n m"   . numbers-math)
  ("<f12> n t"   . numbers-trivia)
  ("<f12> n C-m" . numbers-random-math)
  ("<f12> n C-t" . numbers-random-trivia)
  ("<f12> n n"   . numbers-random))
