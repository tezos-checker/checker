#!/usr/bin/env ruby

require 'etc'
require 'json'
require 'open3'
require 'enumerator'
require 'ostruct'

LIGO_DIR="#{__dir__}/../generated/ligo"
CONTRACT_TARGET="#{__dir__}/../generated/michelson/main.tz"
FUNCTIONS_TARGET="#{__dir__}/../generated/michelson/functions.json"

MAIN_FILE="#{LIGO_DIR}/main.mligo"

PROTOCOL = "PsFLoren"
protocol_arg = ["--protocol", PROTOCOL]

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
output, err, status = Open3.capture3("tezos-client", *protocol_arg, "convert", "data", compiled_contract, "from", "michelson", "to", "binary")
status.success? or raise "tezos-client convert to binary failed:\n#{output}, #{err}"
puts "  ~#{output.length / 2} bytes"

puts "Compiling the initial storage and entrypoints."
entrypoints = File.read("#{LIGO_DIR}/checkerEntrypoints.mligo")
  .scan(/let lazy_id_(\S+) *= \(*(\d*)\)/)
  .map { |g| { name: g[0], fn_id: g[1] }}

initial_storage = nil
packed_entrypoints = []

threads = []
threads << Thread.new {
  stdout, stderr, exit_status = Open3.capture3(
    "ligo", "compile-storage",
    MAIN_FILE, "main",
    "initial_wrapper (\"#{ADDRESS_PLACEHOLDER}\": address)"
  )
  exit_status.success? or raise "compiling initial_wrapper failed.\nstdout:\n#{stdout}\nstderr\n#{stderr}"
  initial_storage = stdout
}

entrypoints.each_slice(entrypoints.length / Etc.nprocessors) { |batch|
  threads << Thread.new {
    batch.each { |entrypoint|
      stdout, stderr, exit_status = Open3.capture3(
        "ligo", "compile-expression", "cameligo",
        "--init-file", MAIN_FILE,
        "Bytes.pack lazy_fun_#{entrypoint[:name]}"
      )
      exit_status.success? or raise "compiling entrypoint #{entrypoint[:name]} failed.\nstdout:\n#{stdout}\nstderr\n#{stderr}"
      packed_entrypoints << {
        name: entrypoint[:name],
        fn_id: entrypoint[:fn_id],
        bytes: stdout.delete_prefix("0x")
      }
    }
  }
}

threads.each(&:join)

chunked_entrypoints = packed_entrypoints.map do |i|
  bytes = i[:bytes]
  size = bytes.length / 2
  chunks = bytes.chars.each_slice(32000).map(&:join)
  puts "#{i[:name].rjust(36)}: ~#{size} bytes, #{chunks.length} chunks"
  { name: i[:name], fn_id: i[:fn_id], chunks: chunks }
end

functions_json = {
  lazy_functions: chunked_entrypoints,
  initial_storage: initial_storage,
  initial_storage_address_placeholder: ADDRESS_PLACEHOLDER
}

functions_json = JSON.pretty_generate(functions_json)

File.write(CONTRACT_TARGET, compiled_contract)
puts "Wrote #{CONTRACT_TARGET}"
File.write(FUNCTIONS_TARGET, functions_json)
puts "Wrote #{FUNCTIONS_TARGET}"

puts "Done."
