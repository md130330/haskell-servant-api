# College-basketball-api

A toy project for me to use Haskell to implement a real world program, namely a REST API that pulls from a db that displays a college basketball teams roster.

## Requirements

You should have both postgres and the AWS SDK installed.

## Setup

In order to make sure everything goes smoothly, after installing postgres
  1. Add the folder that has `ps_config.exe` to your path
  2. If needed, add the lib folder of the postgres application to your path as well 

From there, please run the sql command in the table.txt file to create the table and import the csv into the table created.

Once that is complete, open up the repl and run the following commands

  setSecret "host" {db host url}
  setSecret "port" {db port number}
  setSecret "user" {db username}
  setSecret "dbname" {db name}
  setSecret "password" {db password}
 
This will store your db information in AWS's SSM.
 
From there, just run main from the repl to run locally
