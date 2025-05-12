#!/bin/bash

set -e

echo "🔍 Checking for Terraform installation..."

if ! command -v terraform &> /dev/null; then
  echo "⚠️  Terraform not found. Installing..."

  TERRAFORM_VERSION="1.7.5"
  TMP_DIR=$(mktemp -d)

  cd $TMP_DIR
  wget https://releases.hashicorp.com/terraform/${TERRAFORM_VERSION}/terraform_${TERRAFORM_VERSION}_linux_amd64.zip
  unzip terraform_${TERRAFORM_VERSION}_linux_amd64.zip
  sudo mv terraform /usr/local/bin/

  cd -
  rm -rf $TMP_DIR

  echo "✅ Terraform installed successfully."
else
  echo "✅ Terraform is already installed: $(terraform version | head -n 1)"
fi

echo ""
echo "🚀 Initializing Terraform in infra/ directory..."

cd infra

terraform init

echo ""
read -p "❓ Would you like to run 'terraform apply' now? [y/N]: " answer

if [[ "$answer" == "y" || "$answer" == "Y" ]]; then
  terraform apply
else
  echo "💡 Skipping apply. You can run 'cd infra && terraform apply' later."
fi
