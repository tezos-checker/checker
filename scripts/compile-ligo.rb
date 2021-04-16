#!/usr/bin/env ruby

require 'etc'
require 'json'
require 'open3'
require 'enumerator'

LIGO_DIR="#{__dir__}/../generated/ligo"
CONTRACT_TARGET="#{__dir__}/../generated/michelson/main.tz"
FUNCTIONS_TARGET="#{__dir__}/../generated/michelson/functions.json"

MAIN_FILE="#{LIGO_DIR}/main.mligo"

# this address is created with a key of 20 bytes of zeroes:
#
# import hashlib
# import base58
# alphabet = b"123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
# prefix = bytes([6, 161, 159])
# content = bytes([0] * 20)
# checksum = hashlib.sha256(hashlib.sha256(prefix + content).digest()).digest()[:4]
# print(base58.b58encode(prefix + content + checksum, alphabet=alphabet))
ADDRESS_PLACEHOLDER="tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU"

puts "Compiling contract."

# Compile the main contract
compiled_contract, exit_status = Open3.capture2("ligo", "compile-contract", MAIN_FILE, "main")
exit_status.success? or raise "compile-contract failed:\n#{compiled_contract}"

# Convert the contract to binary to measure the size.
# (we don't want to generate it as binary because it's nice to have it human-readable)
output, err, status = Open3.capture3("tezos-client", "convert", "script", compiled_contract, "from", "michelson", "to", "binary")
status.success? or raise "tezos-client convert failed:\n#{output}, #{err}"
puts "  ~#{output.length / 2} bytes"

puts "Compiling the initial storage and entrypoints."
entrypoints = File.readlines("#{LIGO_DIR}/checkerEntrypoints.mligo").map do |line|
  case line
  when /let lazy_id_(\S+)/ then $1
  end
end.compact

# Extract the initial storage and entrypoint related data
entrypoints_scripts =
  "[ " + entrypoints
          .map {|i| "(\"#{i}\", lazy_id_#{i}, Bytes.pack lazy_fun_#{i})" }
          .join("; ") + " ]"
script = "(initial_wrapper (\"#{ADDRESS_PLACEHOLDER}\": address), #{entrypoints_scripts})"

output, exit_status = Open3.capture2("ligo", "compile-expression", "cameligo", "--michelson-format", "json", "--init-file", MAIN_FILE, script)
exit_status.success? or raise "Packing failed:\n#{output}"
output = JSON.parse(output)

# we have to convert the initial storage back to michelson format
initial_storage = JSON.generate(output["args"][0])
initial_storage, err, status = Open3.capture3("tezos-client", "convert", "data", initial_storage, "from", "json", "to", "michelson")
status.success? or raise "tezos-client convert failed:\n#{output}, #{err}"
initial_storage.strip!

packed_functions =
  output["args"][1]
    .map { |p|
       name = p["args"][0]["args"][0]["string"]
       id = p["args"][0]["args"][1]["int"]
       bytes = p["args"][1]["bytes"]
       { :name => name, :id => id, :bytes => bytes }
    }

raise "Missing entrypoint data" unless packed_functions.length == entrypoints.length

# Chunk entrypoint data
lazy_functions = packed_functions.map do |i|
  bytes = i[:bytes]
  size = bytes.length / 2
  chunks = bytes.chars.each_slice(32000).map { |c| "#{c.join("")}" }
  puts "#{i[:name].rjust(36)}: ~#{size} bytes, #{chunks.length} chunks"
  { :name => i[:name], :id => i[:id], :chunks => chunks }
end

functions_json = {
  :lazy_functions => lazy_functions,
  :initial_storage => initial_storage,
  :initial_storage_address_placeholder => ADDRESS_PLACEHOLDER
}

functions_json = JSON.pretty_generate(functions_json)

File.write(CONTRACT_TARGET, compiled_contract)
puts "Wrote #{CONTRACT_TARGET}"
File.write(FUNCTIONS_TARGET, functions_json)
puts "Wrote #{FUNCTIONS_TARGET}"

puts "Done."
