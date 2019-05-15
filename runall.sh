for f in $(find . -name '*.impf'); do echo "\n\n###### $f ###### \n"; ./Main -v $f; done
