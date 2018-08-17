all:cp compile

cp:
	mkdir -p ./ebin
	cp ./src/cache.app.src ./ebin/cache.app

compile:
	erl -pa ./ebin -I ./include -o ./ebin -noinput \
		-eval "case make:all() of up_to_date -> halt(0); _ -> halt(1) end"

clean:
	rm ./ebin/*
