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

##########################
puts "Compiling contract."
##########################

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

###########################
puts "Compiling the views."
###########################

views = File.read("#{LIGO_DIR}/checkerEntrypoints.mligo")
  .scan(/let wrapper_view_(\S+) *\([^:]*: *(.*) \* wrapper\): *([^=]*)/)
  .map { |g| { name: g[0], param_ty: g[1].strip, return_ty: g[2].strip }}

def compile_type_json(type)
  # TZIP-16 requires us to specify the argument and the return type of views, however
  # ligo does not have a compile-type command. So, we use UNPACK to make the type appear
  # in the generated michelson and grab the type from there.
  stdout, stderr, exit_status = Open3.capture3(
    "ligo", "compile-expression", "cameligo",
    "--init-file", MAIN_FILE,
    "--michelson-format", "json",
    "fun (i: bytes) -> (Bytes.unpack i: (#{type}) option)"
  )
  exit_status.success? or raise "compiling type #{type} failed.\nstdout:\n#{stdout}\nstderr\n#{stderr}"
  obj = JSON.parse(stdout)
  obj[0]["args"][0]
end

def compile_code_json(expr)
  stdout, stderr, exit_status = Open3.capture3(
    "ligo", "compile-expression", "cameligo",
    "--init-file", MAIN_FILE,
    "--michelson-format", "json",
    expr
  )
  exit_status.success? or raise "compiling expression #{expr} failed.\nstdout:\n#{stdout}\nstderr\n#{stderr}"
  JSON.parse(stdout)
end

packed_views = []

threads = []
views.each_slice([views.length / Etc.nprocessors, 1].max) { |batch|
  threads << Thread.new {
    batch.each { |view|
      packed_views << {
        :name => view[:name],
        :parameter => compile_type_json(view[:param_ty]),
        :returnType => compile_type_json(view[:return_ty]),
        :code => compile_code_json("wrapper_view_#{view[:name]}")
      }
    }
  }
}
threads.each(&:join)

#################################
puts "Compiling the entrypoints."
#################################

entrypoints = File.read("#{LIGO_DIR}/checkerEntrypoints.mligo")
  .scan(/let lazy_id_(\S+) *= \(*(\d*)\)/)
  .map { |g| { name: g[0], fn_id: g[1] }}

packed_entrypoints = []

threads = []
entrypoints.each_slice([entrypoints.length / Etc.nprocessors, 1].max) { |batch|
  threads << Thread.new {
    batch.each { |entrypoint|
      stdout, stderr, exit_status = Open3.capture3(
        "ligo", "compile-expression", "--michelson-format=hex", "cameligo",
        "--init-file", MAIN_FILE,
        "lazy_fun_#{entrypoint[:name]}"
      )
      exit_status.success? or raise "compiling entrypoint #{entrypoint[:name]} failed.\nstdout:\n#{stdout}\nstderr\n#{stderr}"
      packed_entrypoints << {
        name: entrypoint[:name],
        fn_id: entrypoint[:fn_id],
        bytes: stdout.strip().delete_prefix("0x")
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

########################################
puts "Extracting the token information."
########################################

stdout, stderr, exit_status = Open3.capture3(
  "ligo", "compile-expression", "cameligo",
  "--init-file", MAIN_FILE, "--michelson-format", "json",
  '''Map.literal
       [ ("kit_token_id", kit_token_id)
       ; ("lqt_token_id", lqt_token_id)
       ; ("kit_decimal_digits", kit_decimal_digits)
       ; ("lqt_decimal_digits", lqt_decimal_digits)
       ]
  '''
)

exit_status.success? or raise "extracting token info failed.\nstdout:\n#{stdout}\nstderr\n#{stderr}"
token_info = JSON.parse(stdout).to_h { |i| [ i["args"][0]["string"], i["args"][1]["int"].to_i ] }

#########################
puts "Saving the result."
#########################

functions_json = {
  lazy_functions: chunked_entrypoints,
  views: packed_views,
  token_info: token_info
}

functions_json = JSON.pretty_generate(functions_json)

system("mkdir", "-p", MICHELSON_DIR)
File.write(CONTRACT_TARGET, compiled_contract)
puts "Wrote #{CONTRACT_TARGET}"
File.write(FUNCTIONS_TARGET, functions_json)
puts "Wrote #{FUNCTIONS_TARGET}"

puts "Done."
