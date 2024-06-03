# Simona Ceskova xcesko00
# FLP projekt 1
# 30.03.2024
	
NAME = flp-fun 
NAMEHS = flp-fun.hs 
CC = ghc -Wall

flp-fun: $(NAMEHS)
	$(CC) -o $(NAME) $(NAMEHS)
	
clean:
	rm -f $(NAME)/*.o *.hi
