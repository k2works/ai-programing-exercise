package main

import (
	"log"
	"os"

	"github.com/joho/godotenv"
	"github.com/k2works/sales-management-db/pkg/database"
)

func main() {
	// Load .env file
	if err := godotenv.Load(); err != nil {
		log.Println("Warning: .env file not found")
	}

	// Connect to database
	config := database.NewConfig()
	db, err := database.Connect(config)
	if err != nil {
		log.Fatalf("Database connection error: %v", err)
		os.Exit(1)
	}
	defer db.Close()

	log.Println("Sales Management Server started successfully")
	log.Println("Connected to database")

	// TODO: Add HTTP server and other logic here
}
