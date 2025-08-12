# Mannschafts-Organisations-Webapp - Funktionsdokumentation

## Projektübersicht

Eine Webapp zur Organisation einer Sportmannschaft, die Mannschaftsverwaltung und Verfügbarkeitsverfolgung ohne komplexe Authentifizierung ermöglicht.

**Sprache:** Deutsch

## Core Features

### 1. Team Creation & Access

- **Team Creation**

  - Simple team creation form (team name, sport, basic info)
  - Generate team slug from name (e.g., "Dragons FC" → "dragons-fc")
  - Combine slug with unique identifier (UUID) for security
  - No registration required - just create and go

- **Low-Friction Access**

  - Team access via unique URL with team name slug (e.g., `/team/dragons-fc-abc123-def456-ghi789`)
  - Human-readable URLs that are still secure and unpredictable
  - Bookmark-friendly URLs with built-in "security"
  - Share team link with members for instant access
  - No passwords or accounts needed

- **Team Settings**
  - Basic team information (name, sport, season)
  - Team admin controls (who can edit matches/roster)
  - Option to regenerate team ID if needed

### 2. Match Management

- **Match List Display**

  - Show upcoming matches in chronological order
  - Display opponent team name
  - Show match location (Home/Away indicator)
  - Display match date and time
  - Visual distinction between home and away matches

- **Match Information**

  - Match details (date, time, opponent, venue)
  - Ability to add/edit match information (admin functionality)
  - Past matches archive

- **Match Date Changes**
  - Admin can modify match dates and times
  - Automatic reset of all team member availability when date changes
  - Notification to all team members about the date change
  - Clear indication that availability needs to be updated

### 3. Team Member Management

- **Team Roster**
  - Display all team members by name
  - Simple list view of active team members
  - Ability to add/remove team members (admin functionality)

### 4. Availability Tracking

- **Per-Match Availability**

  - Each team member can indicate availability for specific matches
  - Simple Yes/No/Maybe availability options
  - Visual overview of who is available for each match
  - Real-time updates when members update their availability

- **Availability Interface**
  - Easy-to-use interface for members to update their status
  - Quick overview of all upcoming matches for a member
  - Bulk availability setting for multiple matches

### 5. Overview & Dashboard

- **Match Overview**

  - Combined view showing matches with availability counts
  - Quick glance at team strength for upcoming matches
  - Identify matches that need attention (low availability)

- **Team Availability Summary**
  - Overview of which members are most/least available
  - Patterns in availability (helpful for planning)

## Technical Requirements

### User Experience

- **No-friction Access**

  - No login/registration required initially
  - Members can access via simple link or bookmark
  - Minimal clicks to update availability

- **Mobile-friendly**
  - Responsive design for phone/tablet use
  - Touch-friendly interface elements
  - Works well on various screen sizes

### Data Management

- **Simple Data Model**

  - Teams: name, slug, UUID, sport, settings
  - Matches: date, opponent, home/away, venue
  - Members: name, contact info (optional)
  - Availability: member + match + status

- **URL Structure**
  - Team URLs: `/team/{slug}-{uuid}` (e.g., `/team/dragons-fc-abc123-def456-ghi789`)
  - Slug generation: lowercase, spaces to hyphens, remove special characters
  - UUID ensures uniqueness and security even with duplicate team names

### Future Authentication Considerations

- **Deferred Authentication**
  - Current solution: open access for all team members
  - Future: simple team-based access (team code/link)
  - Potential: individual member identification without accounts
  - Consider: magic links, phone number verification, or team invitation system

## User Stories

### As a Team Creator

- I want to create a new team with just a name and basic info
- I want to get a unique team link that I can share with my team members
- I want team members to access the team without any registration or login
- I want to bookmark the team URL for easy access

### As a Team Member

- I want to see all upcoming matches at a glance
- I want to quickly mark my availability for each match
- I want to see who else is available for matches
- I want to update my availability from my phone easily

### As a Team Organizer/Captain

- I want to add new matches to the schedule
- I want to see availability overview for each match
- I want to identify matches where we might have low attendance
- I want to manage the team roster (add/remove members)
- I want to change the date of a match and automatically reset all team member availability for that match
- I want all team members to be notified when a match date changes so they can update their availability

### As a Coach/Manager

- I want to see team availability patterns
- I want to plan lineups based on availability
- I want to communicate with available players for specific matches

## Implementation Phases

### Phase 1: Basic Functionality

- Implement team creation with unique ID generation
- Create team access via unique URLs
- Create match list with basic info
- Add team member roster
- Implement simple availability marking
- Basic responsive UI with Tailwind

### Phase 2: Enhanced UX

- Improve visual design and user experience
- Add match details and additional information
- Implement better overview/dashboard views
- Mobile optimization

### Phase 3: Advanced Features

- Team statistics and availability patterns
- Export functionality (lineups, contact lists)
- Integration possibilities (calendar, messaging)

### Phase 4: Access Control (Future)

- Implement chosen authentication solution
- Team management and privacy controls
- Member role management

## Technical Stack

- **Frontend & Backend**: Lamdera (Elm)
- **Styling**: Tailwind CSS
- **Data Storage**: Lamdera's built-in persistence
- **Real-time Updates**: Lamdera's automatic synchronization

## Success Metrics

- Easy for team members to update availability (< 30 seconds)
- Clear visibility of team availability for each match
- Mobile-friendly usage
- Minimal maintenance overhead for team organizers
