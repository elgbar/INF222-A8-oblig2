for f in $(find . -name '*.impf'); do echo "\n\n###### $f ###### \n"; ./Main $f; done
