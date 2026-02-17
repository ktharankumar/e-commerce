#!/bin/bash
# ============================================================
# Pull latest images from Docker Hub and restart on EC2
#
# USAGE: cd ~/Shoppers/infra && ./deploy-ec2.sh
# ============================================================

set -e

echo ""
echo "=== Pulling latest images from Docker Hub ==="
docker compose -f docker-compose.yml -f docker-compose.prod.yml --env-file .env.prod pull

echo ""
echo "=== Stopping old containers ==="
docker compose -f docker-compose.yml -f docker-compose.prod.yml --env-file .env.prod down

echo ""
echo "=== Starting services ==="
docker compose -f docker-compose.yml -f docker-compose.prod.yml --env-file .env.prod up -d

echo ""
echo "=== Cleaning up old images ==="
docker image prune -f

echo ""
echo "=== Deploy complete! ==="
docker compose -f docker-compose.yml -f docker-compose.prod.yml --env-file .env.prod ps
