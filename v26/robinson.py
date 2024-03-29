def run(first_row):
    rows = [first_row]
    print(first_row)
    while True:
        new_row = next_row(rows[-1])
        print(new_row)
        if new_row in rows:
            break
        rows.append(new_row)

def next_row(row):
    result = [0] * 10
    for n in row:
        for digit in str(n):
            result[ord(digit) - ord('0')] += 1
    return result


if __name__ == '__main__':
    run([14, 2, 8, 4, 43, 6, 13, 101, 0, 6])
