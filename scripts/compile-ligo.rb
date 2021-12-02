#!/usr/bin/env ruby

require 'etc'
require 'json'
require 'open3'
require 'enumerator'
require 'ostruct'

LIGO_DIR="#{__dir__}/../generated/ligo"
MICHELSON_DIR="#{__dir__}/../generated/michelson"

MAIN_FILE="#{LIGO_DIR}/main.mligo"
MAIN_CONTRACT_TARGET="#{MICHELSON_DIR}/main.tz"
FUNCTIONS_TARGET="#{MICHELSON_DIR}/functions.json"

WTEZ_FILE="#{LIGO_DIR}/wtezMain.mligo"
WTEZ_CONTRACT_TARGET="#{MICHELSON_DIR}/wtezMain.tz"
WTEZ_METADATA_TARGET="#{MICHELSON_DIR}/wtez_metadata.json"

WCTEZ_FILE="#{LIGO_DIR}/wctezMain.mligo"
WCTEZ_CONTRACT_TARGET="#{MICHELSON_DIR}/wctezMain.tz"
WCTEZ_METADATA_TARGET="#{MICHELSON_DIR}/wctez_metadata.json"

MOCK_FA2_FILE="#{LIGO_DIR}/mockFA2Main.mligo"
MOCK_FA2_CONTRACT_TARGET="#{MICHELSON_DIR}/mockFA2Main.tz"
MOCK_FA2_METADATA_TARGET="#{MICHELSON_DIR}/mock_fa2_metadata.json"

PROTOCOL = "PsFLoren"
protocol_arg = ["--protocol", PROTOCOL]

###################################
puts "Compiling the main contract."
###################################

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

##########################################
puts "Compiling the tez wrapper contract."
##########################################

compiled_wtez_contract, exit_status = Open3.capture2("ligo", "compile-contract", WTEZ_FILE, "main")
exit_status.success? or raise "compile-contract failed:\n#{compiled_wtez_contract}"

begin
  # Convert the contract to binary to measure the size.
  # (we don't want to generate it as binary because it's nice to have it human-readable)
  output, err, status = Open3.capture3("tezos-client", *protocol_arg, "convert", "data", compiled_wtez_contract, "from", "michelson", "to", "binary")
rescue
  puts "  Can't run tezos-client, skipping measurement."
else
  status.success? or raise "tezos-client convert to binary failed:\n#{output}, #{err}"
  puts "  ~#{output.length / 2} bytes"
end

##########################################
puts "Compiling the wctez contract."
##########################################

compiled_wctez_contract, exit_status = Open3.capture2("ligo", "compile-contract", WCTEZ_FILE, "main")
exit_status.success? or raise "compile-contract failed:\n#{compiled_wctez_contract}"

begin
  # Convert the contract to binary to measure the size.
  # (we don't want to generate it as binary because it's nice to have it human-readable)
  output, err, status = Open3.capture3("tezos-client", *protocol_arg, "convert", "data", compiled_wctez_contract, "from", "michelson", "to", "binary")
rescue
  puts "  Can't run tezos-client, skipping measurement."
else
  status.success? or raise "tezos-client convert to binary failed:\n#{output}, #{err}"
  puts "  ~#{output.length / 2} bytes"
end

##########################################
puts "Compiling the mock FA2 contract."
##########################################

compiled_mock_fa2_contract, exit_status = Open3.capture2("ligo", "compile-contract", MOCK_FA2_FILE, "main")
exit_status.success? or raise "compile-contract failed:\n#{compiled_mock_fa2_contract}"

begin
  # Convert the contract to binary to measure the size.
  # (we don't want to generate it as binary because it's nice to have it human-readable)
  output, err, status = Open3.capture3("tezos-client", *protocol_arg, "convert", "data", compiled_mock_fa2_contract, "from", "michelson", "to", "binary")
rescue
  puts "  Can't run tezos-client, skipping measurement."
else
  status.success? or raise "tezos-client convert to binary failed:\n#{output}, #{err}"
  puts "  ~#{output.length / 2} bytes"
end

###########################
puts "Compiling the views."
###########################

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

#####################################
puts "Compiling the views (checker)."
#####################################

checker_views = File.read("#{LIGO_DIR}/checkerEntrypoints.mligo")
  .scan(/let wrapper_view_(\S+) *\([^:]*: *(.*) \* wrapper\) *: *([^=]*)/)
  .map { |g| { name: g[0], param_ty: g[1].strip, return_ty: g[2].strip }}

packed_checker_views = []

threads = []
checker_views.each_slice([checker_views.length / Etc.nprocessors, 1].max) { |batch|
  threads << Thread.new {
    batch.each { |view|
      packed_checker_views << {
        :name => view[:name],
        :parameter => compile_type_json(view[:param_ty]),
        :returnType => compile_type_json(view[:return_ty]),
        :code => compile_code_json("wrapper_view_#{view[:name]}")
      }
    }
  }
}
threads.each(&:join)

##################################
puts "Compiling the views (wtez)."
##################################

wtez_views = File.read("#{LIGO_DIR}/wtez.mligo")
  .scan(/let view_(\S+) *\([^:]*: *(.*) \* wtez_state\) *: *([^=]*)/)
  .map { |g| { name: g[0], param_ty: g[1].strip, return_ty: g[2].strip }}

packed_wtez_views = []

threads = []
wtez_views.each_slice([wtez_views.length / Etc.nprocessors, 1].max) { |batch|
  threads << Thread.new {
    batch.each { |view|
      packed_wtez_views << {
        :name => view[:name],
        :parameter => compile_type_json(view[:param_ty]),
        :returnType => compile_type_json(view[:return_ty]),
        :code => compile_code_json("view_#{view[:name]}")
      }
    }
  }
}
threads.each(&:join)

wtez_metadata_json = {
  views: packed_wtez_views,
}
wtez_metadata_json = JSON.pretty_generate(wtez_metadata_json)

###################################
puts "Compiling the views (wctez)."
###################################

wctez_views = File.read("#{LIGO_DIR}/wctez.mligo")
  .scan(/let view_(\S+) *\([^:]*: *(.*) \* wctez_state\) *: *([^=]*)/)
  .map { |g| { name: g[0], param_ty: g[1].strip, return_ty: g[2].strip }}

packed_wctez_views = []

threads = []
wctez_views.each_slice([wctez_views.length / Etc.nprocessors, 1].max) { |batch|
  threads << Thread.new {
    batch.each { |view|
      packed_wctez_views << {
        :name => view[:name],
        :parameter => compile_type_json(view[:param_ty]),
        :returnType => compile_type_json(view[:return_ty]),
        :code => compile_code_json("view_#{view[:name]}")
      }
    }
  }
}
threads.each(&:join)

wctez_metadata_json = {
  views: packed_wctez_views,
}
wctez_metadata_json = JSON.pretty_generate(wctez_metadata_json)

#####################################
puts "Compiling the views (mockFA2)."
#####################################

mock_fa2_views = File.read("#{LIGO_DIR}/mockFA2.mligo")
  .scan(/let view_(\S+) *\([^:]*: *(.*) \* mock_fa2_state\) *: *([^=]*)/)
  .map { |g| { name: g[0], param_ty: g[1].strip, return_ty: g[2].strip }}

packed_mock_fa2_views = []

threads = []
mock_fa2_views.each_slice([mock_fa2_views.length / Etc.nprocessors, 1].max) { |batch|
  threads << Thread.new {
    batch.each { |view|
      packed_mock_fa2_views << {
        :name => view[:name],
        :parameter => compile_type_json(view[:param_ty]),
        :returnType => compile_type_json(view[:return_ty]),
        :code => compile_code_json("view_#{view[:name]}")
      }
    }
  }
}
threads.each(&:join)

mock_fa2_metadata_json = {
  views: packed_mock_fa2_views,
}
mock_fa2_metadata_json = JSON.pretty_generate(mock_fa2_metadata_json)

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
        "ligo", "compile-expression", "cameligo",
        "--init-file", MAIN_FILE,
        "Bytes.pack lazy_fun_#{entrypoint[:name]}"
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

#########################
puts "Saving the result."
#########################

functions_json = {
  lazy_functions: chunked_entrypoints,
  views: packed_checker_views,
}
functions_json = JSON.pretty_generate(functions_json)

system("mkdir", "-p", MICHELSON_DIR)

File.write(MAIN_CONTRACT_TARGET, compiled_contract)
puts "Wrote #{MAIN_CONTRACT_TARGET}"
File.write(FUNCTIONS_TARGET, functions_json)
puts "Wrote #{FUNCTIONS_TARGET}"

File.write(WTEZ_CONTRACT_TARGET, compiled_wtez_contract)
puts "Wrote #{WTEZ_CONTRACT_TARGET}"
File.write(WTEZ_METADATA_TARGET, wtez_metadata_json)
puts "Wrote #{WTEZ_METADATA_TARGET}"

File.write(WCTEZ_CONTRACT_TARGET, compiled_wctez_contract)
puts "Wrote #{WCTEZ_CONTRACT_TARGET}"
File.write(WCTEZ_METADATA_TARGET, wctez_metadata_json)
puts "Wrote #{WCTEZ_METADATA_TARGET}"

File.write(MOCK_FA2_CONTRACT_TARGET, compiled_mock_fa2_contract)
puts "Wrote #{MOCK_FA2_CONTRACT_TARGET}"
File.write(MOCK_FA2_METADATA_TARGET, mock_fa2_metadata_json)
puts "Wrote #{MOCK_FA2_METADATA_TARGET}"

puts "Done."
