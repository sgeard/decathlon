! A Fortran solution to the decathlon programming challenge.
!
! Written by Simon Geard, August 2016
!
! Not eligible of course because of the weird language constraints but if you're
! forward thinking and open minded like me you'll at least be interested in this
! entry.
!
! Tested with gfortran 6.1 and ifort 17 beta
!
! Build
! -----
!    gfortran -o Decathlon -Ofast Decathlon.f90
!
! Run
! ---
!    ./Decathlon
!
! Testing
! -------
!   Wrote a separate program to create data.
!
! The time for 1000 competitors at 100 meeting was < 10s 
!  
!==============================================================================

! Supporting module to provide the application with a string object which can
! then be wrapped into an allocatable array.
module string
    implicit none
    
    type string_t
        character(len=:), allocatable :: str
    contains
        generic, public    :: operator(==) => equal_strings
        procedure, private :: equal_strings
        generic, public    :: assignment(=) => set_strings_c
        procedure, private :: set_strings_c
        procedure, public  :: to_upper => to_upper_st
    end type string_t

contains
   
    ! Implementation code for string_t    
    pure subroutine set_strings_c(this, str)
        class(string_t), intent(inout) :: this
        character(len=*), intent(in)   :: str
        this%str = str
    end subroutine set_strings_c

    pure function equal_strings(s1, s2) result(r)
        logical :: r
        class(string_t), intent(in) :: s1
        type(string_t), intent(in)  :: s2
        r = (s1%str == s2%str)
    end function equal_strings
    
    pure function to_upper_st(this) result(r)
        type(string_t) :: r
        class(string_t), intent(in) :: this
        r%str = to_upper(this%str)
    end function to_upper_st
    
    ! Map string to upper-case
    pure function to_upper(str) result(r)
        character(len=:), target, allocatable :: r
        character(*), intent(in)              :: str
        integer                   :: i
        integer, parameter        :: case_shift = iachar('a')-iachar('A')
        character(len=:), pointer :: c
        r = trim(str)
        do i=1,len_trim(r)
            c => r(i:i)
            if (lge(c,'a') .and. lle(c,'z')) then
                c = achar(iachar(c) - case_shift)
            end if
        end do
        return
    end function to_upper
end module string

!==============================================================================

module decathlon
    use string
    implicit none
    
    public :: create_scores_table, process_decathon_data, decathlon_t
    
    private
    
    type event_t
        integer        :: id = 0
        integer        :: e_type = 0
        type(string_t) :: event
        type(string_t) :: abbreviation
        real(8)        :: a = 0
        real(8)        :: b = 0
        real(8)        :: c = 0
    contains
        procedure, public :: create => create_event
    end type event_t
    type(event_t) :: events_table(10)
    
    ! Scores per person
    type scores_t
        real(8) :: event_score(10) = 0 ! Scores for each event corresponding to the events_table
    end type scores_t
    
    type decathlon_t
        type(scores_t), allocatable :: all_scores(:)
        type(string_t), allocatable :: all_people(:)
        integer                     :: num_people = 0
    contains
        procedure, public :: get_person_index
        procedure, public :: append => append_data
        procedure, public :: clear => clear_data
        procedure, public :: generate_output
    end type decathlon_t
    

contains
    
    ! =========================================================================
    !
    ! Implementation code for event_t       
    subroutine create_event(this, id, e_type, name, abbr, a, b, c)
        class(event_t), intent(inout) :: this
        integer, intent(in)           :: id
        integer, intent(in)           :: e_type
        character(*), intent(in)      :: name
        character(*), intent(in)      :: abbr
        real(8), intent(in)           :: a, b, c
        this%id = id
        this%e_type = e_type
        this%event = name
        this%abbreviation = abbr
        this%a = a
        this%b = b
        this%c = c
    end subroutine create_event
        
    subroutine create_scores_table
        call events_table(1)%create(  1,1,'100m',        '100m',   25.43470d0,18.0d0,1.810d0)
        call events_table(2)%create(  2,1,'110m hurdles','110m',   5.74352d0, 28.5d0,1.92d0)
        call events_table(3)%create(  3,1,'400m',        '400m',   1.53775d0, 82d0,  1.81d0)
        call events_table(4)%create(  4,1,'1500m',       '1500m',  0.03768d0, 480d0, 1.85d0)
        call events_table(5)%create(  5,2,'Discus',      'Discus', 12.91d0,   4d0,   1.1d0)
        call events_table(6)%create(  6,2,'Javelin',     'Javelin',10.14d0,   7d0,   1.08d0)
        call events_table(7)%create(  7,2,'Shot put',    'Shot',   51.39d0,   1.5d0, 1.05d0)
        call events_table(8)%create(  8,3,'Long jump',   'Long',   0.14354d0, 220d0, 1.4d0)
        call events_table(9)%create(  9,3,'High jump',   'High',   0.8465d0,  75d0,  1.42d0)
        call events_table(10)%create(10,3,'Pole vault',  'Pole',   0.2797d0,  100d0, 1.35d0)
    end subroutine create_scores_table
    
    ! =========================================================================

    ! Selection sort so the output can be ordered. Not the fastest method but
    ! not the slowest either and simple to implement.
    pure subroutine sort_descending(s, p)
        integer, intent(inout)        :: s(:)  ! Scores to be sorted
        type(string_t), intent(inout) :: p(:)  ! People to be corrrespondingly sorted
        integer        :: i, j, n
        integer        :: tmp_s
        type(string_t) :: tmp_p
        n = size(s)
        do j=1,n-1
            i = maxloc(s(j+1:n),1) + j
            if (s(i) > s(j)) then
                tmp_s = s(i)
                s(i) = s(j)
                s(j) = tmp_s
                tmp_p = p(i)
                p(i) = p(j)
                p(j) = tmp_p
            end if
        end do
    end subroutine sort_descending
    
    ! For a given event and result calculate the corresponding points score
    pure function calculate_score(event_id, raw_data) result(r)
        integer :: r
        integer, intent(in) :: event_id
        real(8), intent(in) :: raw_data

        r = 0
        if (raw_data == 0) return
        
        ! Caclulate the score based on the event type
        associate(event_type => events_table(event_id)%e_type, &
                  a => events_table(event_id)%a,               &
                  b => events_table(event_id)%b,               &
                  c => events_table(event_id)%c)
            select case(event_type)
            case(1)
                if (b > raw_data) then
                    r = int(a*(b-raw_data)**c)
                end if
            case(2,3)
                if (raw_data > b) then
                    r = int(a*(raw_data-b)**c)
                end if
            case default
                ! Ignore unknown event type,  just return 0
            end select
        end associate
    end function calculate_score

    subroutine process_decathon_data(in_file, out_file)
        use iso_fortran_env
        character(len=*), intent(in)   :: in_file
        character(len=*), intent(in)   :: out_file
        type(decathlon_t)           :: decathlon_data
        integer                     :: uin, uout
        character(len=100)          :: cbuff
        real(8)                     :: score
        type(string_t)              :: tokens(3), person, abbr
        
        open(newunit=uin,file=in_file,action='read',status='old',form='formatted')
        open(newunit=uout,file=out_file,action='write',status='replace',form='formatted')
        all_lines: do
            call decathlon_data%clear
            dataset: do
                read(uin,'(a)') cbuff
                if (cbuff(1:1) == '#') then
                    ! Process the accrued data
                    call decathlon_data%generate_output(uout)
                    ! Read ahead in case this is the end of the data
                    read(uin,'(a)') cbuff
                    if (cbuff(1:2) == '##') exit all_lines ! End of data
                    backspace(uin)
                    exit dataset ! End of dataset
                end if
                
                ! Accrue data
                tokens = tokenize_string(cbuff)
                call extract_from_token(tokens, person, abbr, score)
                call decathlon_data%append(person, abbr, score)
            end do dataset
            write(uout,'(a)')
        end do all_lines
        close(uin)
        close(uout)
    end subroutine process_decathon_data

    ! Clear all decathlon data
    subroutine clear_data(this)
        class(decathlon_t), intent(inout) :: this
        if (allocated(this%all_people)) deallocate(this%all_people)
        if (allocated(this%all_scores)) deallocate(this%all_scores)
        this%num_people = 0
    end subroutine clear_data
        
    ! Add data to the decathlon summary
    subroutine append_data(this, person, abbr, score)
        class(decathlon_t), intent(inout) :: this
        type(string_t), intent(in)        :: person, abbr
        real(8), intent(in)               :: score
        integer                     :: a_idx, p_idx
        type(scores_t), allocatable :: tmp_scores(:)
        type(string_t), allocatable :: tmp_people(:)
        integer, parameter          :: alloc_size = 1000
        
        p_idx = this%get_person_index(person)
        if (p_idx == 0) then
            ! Add a new person
            if (this%num_people == 0) then
                allocate(this%all_scores(alloc_size), this%all_people(alloc_size))
            else if (this%num_people == size(this%all_scores)) then
                allocate(tmp_scores(this%num_people+alloc_size))
                !dir$ parallel
                tmp_scores(1:this%num_people) = this%all_scores
                call move_alloc(tmp_scores, this%all_scores)
                allocate(tmp_people(this%num_people+alloc_size))
                !dir$ parallel
                tmp_people(1:this%num_people) = this%all_people
                call move_alloc(tmp_people, this%all_people)
            end if
            this%num_people = this%num_people + 1
            p_idx = this%num_people
        end if
        a_idx = get_abbreviation_index(abbr)
        this%all_scores(p_idx)%event_score(a_idx) = score
        this%all_people(p_idx) = person
    end subroutine append_data
    
    subroutine generate_output(this, uout)    
        class(decathlon_t), intent(in) :: this
        integer, intent(in)            :: uout
        
        integer                     :: i, event_id
        integer, allocatable        :: overall_score(:)
        type(string_t), allocatable :: all_people(:)
        character(len=20)           :: cbuff
        
        ! Calculate the scores
        allocate(overall_score(this%num_people))
        do i=1, this%num_people
            associate(person=>this%all_people(i), scores=>this%all_scores(i), s=>overall_score(i))
                s = 0
                do event_id=1,size(scores%event_score)
                    s = s + calculate_score(event_id, scores%event_score(event_id))
                end do
            end associate
        end do
        
        ! Sort into descending order
        all_people = this%all_people
        call sort_descending(overall_score, all_people)
        do i=1, this%num_people
            cbuff = adjustl(all_people(i)%str)
            write(uout,'(a20,i5)') cbuff,overall_score(i)
        end do
    end subroutine generate_output
        
    ! Extract the data from a token: competitor's name, event abbreviation and score
    subroutine extract_from_token(token, name, abbrev, score)
        type(string_t), intent(in)  :: token(3)
        type(string_t), intent(out) :: name, abbrev
        real(8), intent(out) :: score
        character(len=20) :: cbuff
        read(token(1)%str,'(a)') cbuff
        name%str = to_upper(trim(cbuff))
        read(token(2)%str,'(a)') cbuff
        abbrev%str = trim(cbuff) 
        read(token(3)%str,*) score
    end subroutine extract_from_token

    ! Extract 3 tokens from the input
    function tokenize_string(in_str) result(r)
        type(string_t) :: r(3)
        character(len=*), intent(in) :: in_str
        character(len=*), parameter :: tab = achar(9)
        character(len=*), parameter :: space = ' '
        integer :: field_end
        integer :: i
        character(len=:), allocatable, target :: line
        character(len=:), pointer :: c

        ! Replace all tabs with spaces
        line = in_str
        do i=1,len(line)
            c => line(i:i)
            if (c == tab) c = space
        end do

        ! Extract 3 tokens, these can be separated by one or more spaces
        field_end = 1
        do i=1,3
            line = adjustl(line(field_end:))
            field_end = scan(line,space)
            if (field_end == 0) field_end = len(line)+1
            r(i) = line(1:field_end-1)
        end do
    end function tokenize_string
    
    ! Reverse lookup, find the id (== index) of a person in the all_people map
    function get_person_index(this, person) result(r)
        integer :: r
        class(decathlon_t), intent(in) :: this
        type(string_t), intent(in)     :: person
        integer :: i
        do i=1, this%num_people
            if (person == this%all_people(i)) then
                r = i
                return
            end if
        end do
        r = 0
    end function get_person_index
    
    ! Reverse lookup, find the event-id given an abbreviation in the events_table
    ! Since the data can be assummed to be correct, i.e. the given abbrev is a valid one,
    ! then we can just use the first letter to determine the jump or throw event
    ! or the first two digits to determine the running event.
    function get_abbreviation_index(abbrev) result(r)
        integer :: r
        type(string_t), intent(in) :: abbrev
        r = 0
        select case(abbrev%str(1:1))
        case('D','d')
            r = 5
        case('J','j')
            r = 6
        case('S','s')
            r = 7
        case('L','l')
            r = 8
        case('H','h')
            r = 9
        case('P','p')
            r = 10
        case default
            select case(abbrev%str(1:2))
            case('10')
                r = 1
            case('11')
                r = 2
            case('40')
                r = 3
            case('15')
                r = 4
            case default
                stop 'unmatched abbreviation'
            end select
        end select
    end function get_abbreviation_index
        
end module decathlon

! =============================================================================

program main
    use iso_fortran_env
    use decathlon
    implicit none

    character(len=*), parameter :: input_file = 'Decathlon.dat'
    character(len=*), parameter :: output_file = 'Decathlon.out'
    logical           :: file_exists
    
    ! Ensure the input file exists
    inquire(file=input_file,exist=file_exists)
    if (.not. file_exists) then
        write(output_unit,'(a)') '***Error: input file '//input_file//' not found'
        stop
    end if

    ! Create the lookup table for calculating the scores
    call create_scores_table

    ! Read the input and calculate the scores
    call process_decathon_data(input_file,output_file)

    ! All done
    stop
end program main
