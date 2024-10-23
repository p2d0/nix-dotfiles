#!/usr/bin/env bash

# Get the public IP address using an external service
PUBLIC_IP=$(curl -s https://api.ipify.org)

curl -v --request PUT \
--url https://6in4.ru/tunnel/a7169468911911efbd6f0242ac120002/24065 \
--header 'Content-Type: application/json' \
--data "{\"ipv4remote\": \"$PUBLIC_IP\"}"
