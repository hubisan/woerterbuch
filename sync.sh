#!/bin/bash

# This script must be executed from the project root directory!
# bash .project/vendor/sync.sh

echo "Fetching the latest AI configurations from GitHub..."
git subtree pull --prefix=.project/vendor https://github.com/hubisan/ai-agents-config.git main --squash

echo "Copying templates to workspace directories..."
cp -f .project/vendor/agents/AGENTS.md .project/agents/
cp -f .project/vendor/tasks/template.org .project/tasks/

echo "All configurations successfully updated!"
