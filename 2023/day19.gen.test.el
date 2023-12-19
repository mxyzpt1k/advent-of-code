;;; code generated from the test input to be evaluated by #'trace19-eval
;;; part of Advent of Code 2023, Day 19

;; px{a<2006:qkq,m>2090:A,rfg}
(define-rule 'px (quote (if (< a 2006)  (apply qkq) (if (> m 2090)  (accept) (apply rfg)))))

;; pv{a>1716:R,A}
(define-rule 'pv (quote (if (> a 1716)  (reject) (accept))))

;; lnx{m>1548:A,A}
(define-rule 'lnx (quote (if (> m 1548)  (accept) (accept))))

;; rfg{s<537:gd,x>2440:R,A}
(define-rule 'rfg (quote (if (< s 537)  (apply gd) (if (> x 2440)  (reject) (accept)))))

;; qs{s>3448:A,lnx}
(define-rule 'qs (quote (if (> s 3448)  (accept) (apply lnx))))

;; qkq{x<1416:A,crn}
(define-rule 'qkq (quote (if (< x 1416)  (accept) (apply crn))))

;; crn{x>2662:A,R}
(define-rule 'crn (quote (if (> x 2662)  (accept) (reject))))

;; in{s<1351:px,qqz}
(define-rule 'in (quote (if (< s 1351)  (apply px) (apply qqz))))

;; qqz{s>2770:qs,m<1801:hdj,R}
(define-rule 'qqz (quote (if (> s 2770)  (apply qs) (if (< m 1801)  (apply hdj) (reject)))))

;; gd{a>3333:R,R}
(define-rule 'gd (quote (if (> a 3333)  (reject) (reject))))

;; hdj{m>838:A,pv}
(define-rule 'hdj (quote (if (> m 838)  (accept) (apply pv))))
