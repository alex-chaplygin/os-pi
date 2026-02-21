(handle
 (progn
   (clear-screen)

   (run-tests 'stop-on-fail)
  )
 (error (message)
	(print "error:" message)))
