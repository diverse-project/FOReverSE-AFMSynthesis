scala -classpath "lib/*" target/scala-2.10/afmsynthesis_2.10-0.1.jar output/synthesized/ 2 3 4 true
scala -classpath "lib/*:target/scala-2.10/afmsynthesis_2.10-0.1.jar" foreverse.afmsynthesis.ConfigurationSemanticsChecker input/test-set/complex_ctc.csv output/synthesized/complex_ctc.csv
