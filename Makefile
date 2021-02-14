play: snake.jar
	scala snake.jar

snake.jar: snake.scala
	scalac snake.scala -d snake.jar

clean:
	rm -f snake.jar

.PHONY: play clean
