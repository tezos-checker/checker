#!/usr/bin/env ruby

require 'etc'
require 'json'
require 'open3'
require 'enumerator'
require 'ostruct'

LIGO_DIR="#{__dir__}/../generated/ligo"
MICHELSON_DIR="#{__dir__}/../generated/michelson"
CONTRACT_TARGET="#{MICHELSON_DIR}/main.tz"
FUNCTIONS_TARGET="#{MICHELSON_DIR}/functions.json"

MAIN_FILE="#{LIGO_DIR}/main.mligo"

PROTOCOL = "PsFLoren"
protocol_arg = ["--protocol", PROTOCOL]

puts "Compiling contract."

# Compile the main contract
compiled_contract, exit_status = Open3.capture2("ligo", "compile-contract", MAIN_FILE, "main")
exit_status.success? or raise "compile-contract failed:\n#{compiled_contract}"

begin
  # Convert the contract to binary to measure the size.
  # (we don't want to generate it as binary because it's nice to have it human-readable)
  output, err, status = Open3.capture3("tezos-client", *protocol_arg, "convert", "data", compiled_contract, "from", "michelson", "to", "binary")
rescue
  puts "  Can't run tezos-client, skipping measurement."
else
  status.success? or raise "tezos-client convert to binary failed:\n#{output}, #{err}"
  puts "  ~#{output.length / 2} bytes"
end

puts "Compiling the entrypoints."
entrypoints = File.read("#{LIGO_DIR}/checkerEntrypoints.mligo")
  .scan(/let lazy_id_(\S+) *= \(*(\d*)\)/)
  .map { |g| { name: g[0], fn_id: g[1] }}

packed_entrypoints = []

threads = []
entrypoints.each_slice([entrypoints.length / Etc.nprocessors, 1].max) { |batch|
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
}

functions_json = JSON.pretty_generate(functions_json)

system("mkdir", "-p", MICHELSON_DIR)
File.write(CONTRACT_TARGET, compiled_contract)
puts "Wrote #{CONTRACT_TARGET}"
File.write(FUNCTIONS_TARGET, functions_json)
puts "Wrote #{FUNCTIONS_TARGET}"

puts "Done."
