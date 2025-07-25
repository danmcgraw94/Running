// Get the Sunday date of the current week formatted as "Mon DD"
function getSundayOfCurrentWeek() {
  const today = new Date();
  const day = today.getDay(); // Sunday = 0
  const diff = today.getDate() - day;
  const sunday = new Date(today);
  sunday.setDate(diff);

  const options = { month: 'short', day: 'numeric' };
  return sunday.toLocaleDateString('en-US', options);
}

// Update header text
document.getElementById('training-header').textContent =
  `Training Plan – Week of ${getSundayOfCurrentWeek()}`;

// Function to add rows to the table
function displayTrainingData(data) {
  const tbody = document.getElementById('table-body');
  tbody.innerHTML = ''; // clear previous rows

  data.forEach(row => {
    const tr = document.createElement('tr');

    // Escape values for safety (optional)
    const date = row.date || '';
    const day = row.day || '';
    const workout = row.workout || '';
    const miles = row.miles || '';

    tr.innerHTML = `
      <td>${date}</td>
      <td>${day}</td>
      <td>${workout}</td>
      <td>${miles}</td>
    `;

    tbody.appendChild(tr);
  });
}

// Load and parse CSV using fetch and PapaParse
fetch('Training_This_Week.csv')
  .then(response => {
    if (!response.ok) throw new Error(`HTTP error! Status: ${response.status}`);
    return response.text();
  })
  .then(csvText => {
    const parsed = Papa.parse(csvText, {
      header: true,
      skipEmptyLines: true,
    });
    displayTrainingData(parsed.data);
  })
  .catch(error => {
    console.error('Error loading or parsing CSV:', error);
    const tbody = document.getElementById('table-body');
    tbody.innerHTML = `<tr><td colspan="4" style="color:red;">Failed to load training data.</td></tr>`;
  });
