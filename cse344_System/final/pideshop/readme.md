
# PideShop

Welcome to the PideShop project, a multi-threaded simulation of a food production and delivery system, a pide (a Turkish pastry) restaurant management. The aim is to simulate the environment of a pide shop where orders are placed by customers and processed by cooks, with ingredients supplied in real-time. 
The multi-threaded server application is implemented i C for managing this Pide shop. The synchronization of common resources is managed using semaphores/mutexes and condition variaables.

## Project Overview

PideShop is designed to manage complex interactions within a pide restaurant through a multi-threaded application. The system handles multiple customer orders simultaneously, ensuring efficient resource management and process synchronization across various threads involving customers, cooks, and suppliers.

## Key Features

- **Multi-threaded Order Handling:** Simultaneously manages multiple customer orders, with separate threads simulating customers, cooks, and suppliers.
- **Dynamic Resource Allocation:** Manages a dynamic inventory of ingredients with real-time updates based on current stock and order needs.
- **Thread Synchronization:** Uses mutexes and semaphores to synchronize operations between cooks and suppliers to prevent data races and ensure consistency.
- **Log Output:** Provides detailed logs of system operations, offering insights into the internal process flows and state changes.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

- Linux or macOS
- GCC compiler

### Installation

Clone the repository to your local machine:

```bash
git clone https://github.com/hilalsaygin/GTUAssignments.git
cd GTUAssignments/cse344_System/final/pideshop
```

Compile the program using:

```bash
make
```

### Running the Application

Execute the program:

```bash
./pideshop
```

## Usage

Edit the `config.txt` file to set up initial conditions like the number of cooks, suppliers, and the initial ingredient stock levels before running the application.

## Contributing

Interested in contributing to the PideShop project? Great! Here's how you can:

1. Fork the repo
2. Create your feature branch (`git checkout -b feature/YourFeature`)
3. Commit your changes (`git commit -am 'Add some YourFeature'`)
4. Push to the branch (`git push origin feature/YourFeature`)
5. Create a new Pull Request

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE) file for details.

## Authors

- **Your Name** - *Initial work* - [YourGithub](https://github.com/yourgithub)

## Acknowledgments

- Hat tip to anyone whose code was used
- Inspiration
- etc
