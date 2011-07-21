grep -E "^Zone\s+\S+\s+\S+\s+\S+\s+|^\s\s\s+[-0-9]" * > zones
grep -E "^Rule\s+\S+\s+\S+\s+\S+\s+" * > rules
