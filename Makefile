.PHONY: check docs test

check:
	find src -type f -name '*.mo' -print0 | xargs -0 /usr/local/bin/moc $(shell vessel sources) --check

all: check-strict docs test

check-strict:
	find src -type f -name '*.mo' -print0 | xargs -0 /usr/local/bin/moc $(shell vessel sources) -Werror --check
docs:
	/usr/local/bin/mo-doc
test:
	make -C test
