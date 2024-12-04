(ns user
  (:require [portal.api :as p]))

(add-tap #'p/submit)

(comment

  (def p (p/open))

  \n
  )
