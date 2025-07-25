// Function to get the Sunday date of the current week, formatted "Mon DD"
function getSundayOfCurrentWeek() {
  const today = new Date();
  const day = today.getDay(); // 0=Sun, 1=Mon, ..., 6=Sat
  const diff = today.getDate() - day; // go back to Sunday
  const sunday = new Date(today);
  sunday.setDate(diff);

  const options = { month: 'short', day: 'numeric' };
  return sunday.toLocaleDateString('en-US', options);
}

// Update header text
document.getElementById('training-header').textContent =
  `Training Plan – Week of ${getSundayOfCurrentWeek()}`;

// Function to populate table body with CSV data rows
function displayTrainingData(data) {
  const tbody = document.getElementById('table-body');
  tbody.innerHTML = ''; // Clear previous rows

  data.forEach(row => {
    const tr = document.createElement('tr');

    tr.innerHTML = `
      <td>${row.date}</td>
      <td>${row.day}</td>
      <td>${row.workout}</td>
      <td>${row.miles}</td>
    `;

    tbody.appendChild(tr);
  });
}

// Fetch and parse CSV, then display
fetch('Training_This_Week.csv')
  .then(res => res.text())
  .then(csvText => {
    const results = Papa.parse(csvText, { header: true, skipEmptyLines: true });
    displayTrainingData(results.data);
  })
  .catch(err => console.error('Error loading CSV:', err));

