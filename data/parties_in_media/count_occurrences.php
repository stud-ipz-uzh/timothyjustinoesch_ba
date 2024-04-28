<?php

$party_occurrences = [];

// find all csv files in __DIR__ except for party_occurrences.csv
$files = glob(__DIR__ . '/*.csv');
$files = array_filter($files, function ($file) {
    return basename($file) !== 'party_occurrences.csv';
});

foreach ($files as $file) {
    // open the file
    echo('Processing ' . $file . PHP_EOL);
    $handle = fopen($file, "r");

    // Get the first row
    $headers = fgetcsv($handle, 0, ",");

    $data = [];
    while (($row = fgetcsv($handle, 0, ",")) !== false) {
        $data[] = array_combine($headers, $row);
    }

    // Convert pubtime into a DateTime object
    foreach ($data as $key => $value) {
        if (!isset($value['pubtime'])) {
            echo('pubtime not set in ' . $file . ' on line ' . $key . PHP_EOL);
            exit;
        }
        $data[$key]['pubtime'] = new DateTime($value['pubtime']);
    }

    // Filter data where pubtime is between 2023-10-21 and 2023-09-22
    $filter_1month = array_filter($data, function ($value) {
        return $value['pubtime'] >= new DateTime('2023-09-22') && $value['pubtime'] <= new DateTime('2023-10-21');
    });

    // filter data where pubtime is between 2023-10-21 and 2023-07-22
    $filter_3month = array_filter($data, function ($value) {
        return $value['pubtime'] >= new DateTime('2023-07-22') && $value['pubtime'] <= new DateTime('2023-10-21');
    });

    // filter data where pubtime is between 2023-10-21 and 2022-04-22
    $filter_6month = array_filter($data, function ($value) {
        return $value['pubtime'] >= new DateTime('2022-04-22') && $value['pubtime'] <= new DateTime('2023-10-21');
    });

    // Count the occurrences
    $count_1month = count($filter_1month);
    $count_3month = count($filter_3month);
    $count_6month = count($filter_6month);

    // Get basename of the file
    $filename = basename($file);

    // Split filename by _ and .
    $filename_parts = explode('_', $filename);
    $filename_parts = explode('.', $filename_parts[1]);
    $party = $filename_parts[0];

    // Add the occurrences to the party_occurrences array
    $party_occurrences[$party] = [
        '1month' => $count_1month,
        '3month' => $count_3month,
        '6month' => $count_6month
    ];

    // Close the file
    fclose($handle);
}

// Write the party_occurrences array to a csv file
$handle = fopen(__DIR__ . '/party_occurrences.csv', 'w');
fputcsv($handle, ['party', '1month', '3month', '6month']);
foreach ($party_occurrences as $party => $occurrences) {
    fputcsv($handle, [$party, $occurrences['1month'], $occurrences['3month'], $occurrences['6month']]);
}
fclose($handle);