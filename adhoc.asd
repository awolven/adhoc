;; Copyright Andrew K. Wolven 2008, 20021
;; This file is released under the GNU General Public License v3
;; See LICENSE.txt in the main directory for details

(defsystem adhoc
  :description "Another Declarative Hierarchical Object-centric CLOS Customization"
  :depends-on (:closer-mop)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :license "GPLv3"
  :components
  ((:file "src/package")
   (:file "src/adhoc")))
