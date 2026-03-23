# EF Coach & Tutors — Business Analytics

This project explores two key questions for a tutoring and executive-function coaching business:

1. **What does the company's financial picture look like, and how many students do we need to hit $12K monthly profit or fund a scholarship program?**
2. **What school characteristics and outreach strategies are most likely to result in engagement and student referrals?**

Using CRM data exported from Streak and R, I built financial models and analyzed outreach patterns across 1,000+ schools to guide business strategy.

---

## Data Source

All outreach and pipeline data was exported from [Streak CRM](https://www.streak.com/) (integrated with Gmail). School tuition estimates were collected for the 2024–2025 school year. Financial figures are based on internal business records from the 2023–2024 academic year.

---

## Files

- `CRM_Data_Editing.R`: Data cleaning, feature engineering, timezone conversion, one-hot encoding, and exploratory visualizations in R.
- `Streak_Export___LS__Trial__7-12-24__12_19_PM__-_CR_Analysis.csv`: Raw CRM export containing school outreach records.
- `Financial_Analysis.pdf`: Slide deck covering revenue/expense breakdown, profit projections, and scenario analysis.
- `School_Relation_Analysis.pdf`: Slide deck covering school engagement findings and referral analysis.

---

## Summary of Findings

### Financial Analysis

- Current estimated monthly profit: **$2,754** across 13 converted students.
- **46 converted students** needed to reach $12K monthly profit; **60** needed to fund a yearly scholarship.
- Shortening trials from 3 weeks to 2 weeks improves profit margin from **15.63% → 18.30%**.
- Raising tutor pay from $45/hr to $50/hr drops margin from **15.63% → 11.82%**.
- Breakpoint conversion rate to offset trial expenses: **17.70%**.

### School Relation Analysis

- Schools with **>$20K tuition** engage and refer students at significantly higher rates.
- Learning Specialists (**22.1%**) and Directors (**21.6%**) had the highest engagement rates.
- Emails sent on **Wednesdays** showed a **23.8%** engagement rate — roughly 2.5× the average.
- Best send times: **9am and 12pm** in the recipient's local timezone.
- Each additional email received from an engaged school increases referral likelihood by **~41.6%** (logistic regression).

---

## Tools Used

- **R**
- **R Libraries: dplyr, lubridate, tidyr, stringr, ggplot2, caret, fastDummies, googlesheets4**
- **Data source: Streak CRM → Google Sheets**
