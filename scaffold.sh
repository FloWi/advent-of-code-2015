#! /bin/zsh

set +e
for i in 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
    do        
        cp -r "src/main/scala/day01" "src/main/scala/day${i}"
        cp -r "src/test/scala/day01" "src/test/scala/day${i}"

        gsed -i "s/01/${i}/" "src/main/scala/day${i}/day01.scala"
        gsed -i "s/01/${i}/" "src/test/scala/day${i}/Day01Test.scala"

        mv "src/main/scala/day${i}/day01.scala" "src/main/scala/day${i}/day${i}.scala"
        mv "src/test/scala/day${i}/Day01Test.scala" "src/test/scala/day${i}/Day${i}Test.scala"
    done
