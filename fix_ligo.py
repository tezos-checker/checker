import os


if __name__ == '__main__':
    dst_dir = './generated/ligo'
    files_list = [os.path.join(dst_dir, f) for f in os.listdir(dst_dir) if f.endswith('mligo')]
    print(f'Found {len(files_list)} files.')

    for file_path in files_list:
        print(f'Fixing {file_path}.')
        with open(file_path) as f:
            initial_text = f.read()
            fixed_text = initial_text.replace(
                'Tezos.amount',
                '(Tezos.get_amount unit)'
            ).replace(
                'Tezos.sender',
                '(Tezos.get_sender unit)'
            ).replace(
                'Tezos.level',
                '(Tezos.get_level unit)'
            ).replace(
                'Tezos.now',
                '(Tezos.get_now unit)'
            ).replace(
                'Tezos.self_address',
                '(Tezos.get_self_address unit)'
            )

        with open(file_path, 'w') as f:
            f.write(fixed_text)
