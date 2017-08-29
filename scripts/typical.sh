#!/bin/bash
sbt -no-colors "testOnly *InventoryTest -- -z \"process multiple requests\"" > res.txt

scala ../../filterlog/src/main/scala/app.scala res.txt trimer [ ] INFO > res1.txt
scala ../../filterlog/src/main/scala/app.scala res1.txt removedatekeeptime 2017 > res2.txt
scala ../../vim_colors/src/main/scala/app.scala "Inventory Publisher" "Transition" > colors.txt
