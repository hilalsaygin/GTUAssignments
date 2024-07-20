# Makefile for PideShop server and HungryVeryMuch client generator

CC = gcc
CFLAGS = -pthread -Wall -Wextra
TARGETS = PideShop HungryVeryMuch

all: $(TARGETS)

PideShop: PideShop.o
	$(CC) $(CFLAGS) -o PideShop PideShop.o

PideShop.o: server/PideShop.c
	$(CC) $(CFLAGS) -c server/PideShop.c

HungryVeryMuch: HungryVeryMuch.o
	$(CC) $(CFLAGS) -o HungryVeryMuch HungryVeryMuch.o

HungryVeryMuch.o: client/HungryVeryMuch.c
	$(CC) $(CFLAGS) -c client/HungryVeryMuch.c

clean:
	rm -f *.o $(TARGETS)

run_server:
	./PideShop $(portnumber) $(CookthreadPoolSize) $(DeliveryPoolSize) $(k)

run_client:
	./HungryVeryMuch $(portnumber) $(numberOfClients) $(p) $(q)
