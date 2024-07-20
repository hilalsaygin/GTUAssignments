# Makefile for PideShop server and HungryVeryMuch client generator

CC = gcc
CFLAGS = -pthread -Wall -Wextra
TARGETS = PideShop HungryVeryMuch

all: $(TARGETS)

PideShop: PideShop.o
	$(CC) $(CFLAGS) -o PideShop PideShop.o

PideShop.o: PideShop.c
	$(CC) $(CFLAGS) -c PideShop.c

HungryVeryMuch: HungryVeryMuch.o
	$(CC) $(CFLAGS) -o HungryVeryMuch HungryVeryMuch.o

HungryVeryMuch.o: HungryVeryMuch.c
	$(CC) $(CFLAGS) -c HungryVeryMuch.c

clean:
	rm -f *.o $(TARGETS)

run_server:
	./PideShop $(portnumber) $(CookthreadPoolSize) $(DeliveryPoolSize) $(k)

run_client:
	./HungryVeryMuch $(portnumber) $(numberOfClients) $(p) $(q)
