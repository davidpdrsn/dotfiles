call spectacular#add_test_runner('cucumber', 'bin/cucumber {spec} -t @focus', '', function("WithCucumberFocusTag"), function("InRailsApp"))
call spectacular#add_test_runner('cucumber', 'cucumber {spec} -t @focus', '', function("WithCucumberFocusTag"))
call spectacular#add_test_runner('cucumber', 'bin/cucumber {spec}', '', function("InRailsApp"))
call spectacular#add_test_runner('cucumber', 'cucumber {spec}', '')
call spectacular#add_test_runner('ruby', 'bin/cucumber', '_steps', function("InRailsApp"))
call spectacular#add_test_runner('ruby', 'cucumber', '_steps')

call spectacular#add_test_runner('ruby', 'bin/rspec {spec} -t @focus', '_spec', function("TestsInRails"), function("WithRspecFocusTag"))
call spectacular#add_test_runner('ruby', 'bin/rspec {spec}', '_spec', function("TestsInRails"))
call spectacular#add_test_runner('ruby', 'rspec {spec} -t @focus', '_spec', function("WithRspecFocusTag"))
call spectacular#add_test_runner('ruby', 'rspec {spec}', '_spec')

call spectacular#add_test_runner('sml', 'smlspec {spec}', '')

call spectacular#add_test_runner('javascript', 'karma run', '_spec')
call spectacular#add_test_runner('javascript', 'karma run', 'Spec')
call spectacular#add_test_runner('coffee', 'karma run', '_spec')
call spectacular#add_test_runner('coffee', 'karma run', 'Spec')

call spectacular#add_test_runner('java', 'bin/test {spec}', 'Test')
" call spectacular#add_test_runner('java', 'javac *.java && junit {spec}', 'Test')
