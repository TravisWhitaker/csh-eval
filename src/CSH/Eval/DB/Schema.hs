{-|
Module      : CSH.Eval.DB.Schema
Description : Schema definition.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.DB.Schema defines and documents the database schema.
-}

{-# OPTIONS_HADDOCK ignore-exports #-}

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RankNTypes #-}

module CSH.Eval.DB.Schema where

import Control.Monad

import qualified Data.Text as T

import Data.List(zipWith4)

import qualified Hasql as H

import qualified Hasql.Postgres as HP

type SchemaInit = forall s. H.Tx HP.Postgres s ()

committee_t :: SchemaInit
committee_t = mapM_ H.unitEx [
    [H.stmt|drop type if exists "committee_t" cascade|]
   ,[H.stmt| create type "committee_t" as enum (
        'evals'
       ,'rnd'
       ,'social'
       ,'history'
       ,'opcomm'
       ,'imps'
       ,'financial'
       ,'chairman'
    )|]
   ]

status_t :: SchemaInit
status_t = mapM_ H.unitEx [
    [H.stmt|drop type if exists "status_t" cascade|]
   ,[H.stmt|create type "status_t" as enum (
        'pending'
       ,'passed'
       ,'failed'
    )|]
   ]

member_t :: SchemaInit
member_t = mapM_ H.unitEx [
    [H.stmt|drop type if exists "member_t" cascade|]
   ,[H.stmt|create type "member_t" as enum (
        'active'
       ,'alumni_good'
       ,'alumni_bad'
       ,'honorary'
       ,'advisory'
       ,'introductory'
       ,'non'
    )|]
   ]

dues_t :: SchemaInit
dues_t = mapM_ H.unitEx [
    [H.stmt|drop type if exists "dues_t" cascade|]
   ,[H.stmt|create type "dues_t" as enum (
        'paid'
       ,'exempt'
    )|]
   ]

event_t :: SchemaInit
event_t = mapM_ H.unitEx [
    [H.stmt|drop type if exists "event_t" cascade|]
   ,[H.stmt|create type "event_t" as enum (
        'house'
       ,'social'
       ,'committee'
       ,'seminar'
       ,'orientation'
    )|]
   ]

project_t :: SchemaInit
project_t = mapM_ H.unitEx [
    [H.stmt|drop type if exists "project_t" cascade|]
   ,[H.stmt|create type "project_t" as enum (
        'major'
    )|]
   ]

eval_t :: SchemaInit
eval_t = mapM_ H.unitEx [
    [H.stmt|drop type if exists "eval_t" cascade|]
   ,[H.stmt|create type "eval_t" as enum (
        'introductory'
       ,'membership'
    )|]
   ]

member :: SchemaInit
member = mapM_ H.unitEx [
    [H.stmt|drop table if exists "member" cascade|]
   ,[H.stmt|create table "member" (
        "id"              bigserial  primary key
       ,"uuid"            uuid       default null constraint "unique_member_uuid" unique
       ,"username"        varchar    not null constraint "unique_member_username" unique
       ,"commonname"      varchar    not null
       ,"password_hash"   bytea      default null
       ,"password_salt"   bytea      default null
       ,"housing_points"  integer    not null default 0
       ,"onfloor_status"  boolean    not null default false
    )|]
   ]

eboard :: SchemaInit
eboard = mapM_ H.unitEx [
    [H.stmt|drop table if exists "eboard" cascade|]
   ,[H.stmt|create table "eboard" (
        "member_id"   bigint       not null
       ,"committee"   committee_t  not null
       ,"start_date"  date         not null
       ,"end_date"    date         default null
       ,constraint "no_simultaneous_eboard_positions" unique ("member_id", "start_date")
    )|]
   ]

room :: SchemaInit
room = mapM_ H.unitEx [
    [H.stmt|drop table if exists "room" cascade|]
   ,[H.stmt|create table "room" (
        "member_id"    bigint   not null
       ,"room_number"  varchar  not null
       ,"start_date"   date     not null
       ,"end_date"     date     default null
       ,constraint "no_simultaneous_member_occupation" unique ("member_id", "start_date")
       ,constraint "no_simultaneous_room_occupation" unique ("room_number", "start_date")
    )|]
   ]

membership :: SchemaInit
membership = mapM_ H.unitEx [
    [H.stmt|drop table if exists "membership" cascade|]
   ,[H.stmt|create table "membership" (
        "member_id"   bigint    not null
       ,"status"      member_t  not null
       ,"start_date"  date      not null
       ,"end_date"    date      default null
       ,constraint "no_simultaneous_membership_status" unique ("member_id", "start_date")
    )|]
   ]

event :: SchemaInit
event = mapM_ H.unitEx [
    [H.stmt|drop table if exists "event" cascade|]
   ,[H.stmt|create table "event" (
        "id"           bigserial    primary key
       ,"title"        varchar      not null
       ,"held"         timestamp    not null
       ,"category"     event_t      not null
       ,"committee"    committee_t  not null
       ,"description"  varchar      not null
       ,constraint "unique_event_title_held" unique ("title", "held")
    )|]
   ]

event_attendee :: SchemaInit
event_attendee = mapM_ H.unitEx [
    [H.stmt|drop table if exists "event_attendee" cascade|]
   ,[H.stmt|create table "event_attendee" (
        "member_id"  bigint   not null
       ,"event_id"   bigint   not null
       ,"host"       boolean  not null default false
       ,constraint "unique_event_attendee" unique ("member_id", "event_id")
    )|]
   ]

project :: SchemaInit
project = mapM_ H.unitEx [
    [H.stmt|drop table if exists "project" cascade|]
   ,[H.stmt|create table "project" (
        "id"            bigserial    primary key
       ,"title"         varchar      not null
       ,"description"   varchar      not null
       ,"submitted"     timestamp    not null
       ,"passed"        timestamp    default null
       ,"committee"     committee_t  not null
       ,"project_type"  project_t    not null
       ,"comments"      varchar      default null
       ,"status"        status_t     not null default 'pending'
    )|]
   ]

project_participant :: SchemaInit
project_participant = mapM_ H.unitEx [
    [H.stmt|drop table if exists "project_participant" cascade|]
   ,[H.stmt|create table "project_participant" (
        "member_id"    bigint   not null
       ,"project_id"   bigint   not null
       ,"description"  varchar  default null
       ,constraint "one_member_per_participant" unique ("member_id", "project_id")
    )|]
   ]

evaluation :: SchemaInit
evaluation = mapM_ H.unitEx [
    [H.stmt|drop table if exists "evaluation" cascade|]
   ,[H.stmt|create table "evaluation" (
        "id"         bigserial  primary key
       ,"member_id"  bigint     not null
       ,"comments"   varchar    default null
       ,"deadline"   timestamp  not null
       ,"available"  boolean    default false
       ,"status"     status_t   not null default 'pending'
       ,"eval_type"  eval_t     not null
    )|]
   ]

conditional :: SchemaInit
conditional = mapM_ H.unitEx [
    [H.stmt|drop table if exists "conditional" cascade|]
   ,[H.stmt|create table "conditional" (
        "id"             bigserial  primary key
       ,"evaluation_id"  bigint     not null constraint "one_conditional_per_eval" unique
       ,"deadline"       timestamp  not null
       ,"description"    varchar    not null
       ,"comments"       varchar    default null
    )|]
   ]

freshman_project :: SchemaInit
freshman_project = mapM_ H.unitEx [
    [H.stmt|drop table if exists "freshman_project" cascade|]
   ,[H.stmt|create table "freshman_project" (
        "id"            bigserial  primary key
       ,"description"   varchar    not null
       ,"project_date"  date       not null
    )|]
   ]

freshman_project_participant :: SchemaInit
freshman_project_participant = mapM_ H.unitEx [
    [H.stmt|drop table if exists "freshman_project_participant" cascade|]
   ,[H.stmt|create table "freshman_project_participant" (
        "freshman_project_id"  bigint    not null
       ,"evaluation_id"        bigint    not null
       ,"eboard"               boolean   not null default false
       ,"status"               status_t  not null default 'pending'
       ,"comments"             varchar   default null
       ,constraint "one_freshman_project_per_eval" unique ("freshman_project_id", "evaluation_id")
    )|]
   ]

packet :: SchemaInit
packet = mapM_ H.unitEx [
    [H.stmt|drop table if exists "packet" cascade|]
   ,[H.stmt|create table "packet" (
        "id"           bigserial  primary key
       ,"member_id"    bigint     not null
       ,"due_date"     date       not null
       ,"percent_req"  integer    not null
       ,constraint "no_simultaneous_packets" unique ("member_id", "due_date")
    )|]
   ]

signature :: SchemaInit
signature = mapM_ H.unitEx [
    [H.stmt|drop table if exists "signature" cascade|]
   ,[H.stmt|create table "signature" (
        "member_id"  bigint     not null
       ,"packet_id"  bigint     not null
       ,"required"   boolean    not null
       ,"signed"     timestamp  default null
       ,constraint "one_signature_per_packet_per_member" unique ("member_id", "packet_id")
    )|]
   ]

queue :: SchemaInit
queue = mapM_ H.unitEx [
    [H.stmt|drop table if exists "queue" cascade|]
   ,[H.stmt|create table "queue" (
        "id"         bigserial  primary key
       ,"member_id"  bigint     not null
       ,"entered"    timestamp  not null
       ,"exited"     timestamp  default null
       ,constraint "no_simultaneous_queue_positions" unique ("member_id", "entered")
    )|]
   ]

application :: SchemaInit
application = mapM_ H.unitEx [
    [H.stmt|drop table if exists "application" cascade|]
   ,[H.stmt|create table "application" (
        "id"         bigserial  primary key
       ,"member_id"  bigint     not null
       ,"created"    timestamp  not null
       ,"status"     status_t   not null default 'pending'
       ,constraint "no_simultaneous_applications" unique ("member_id", "created")
    )|]
   ]

metric :: SchemaInit
metric = mapM_ H.unitEx [
    [H.stmt|drop table if exists "metric" cascade|]
   ,[H.stmt|create table "metric" (
        "id"      bigserial  primary key
       ,"name"    varchar    not null constraint "unique_metric_name" unique
       ,"active"  boolean    default true
    )|]
   ]

reviewer_metric :: SchemaInit
reviewer_metric = mapM_ H.unitEx [
    [H.stmt|drop table if exists "reviewer_metric" cascade|]
   ,[H.stmt|create table "reviewer_metric" (
        "metric_id"    bigint   not null
       ,"reviewer_id"  bigint   not null
       ,"score"        integer  not null
       ,constraint "one_score_per_reviewer_per_metric" unique ("metric_id", "reviewer_id")
    )|]
   ]

interviewer_metric :: SchemaInit
interviewer_metric = mapM_ H.unitEx [
    [H.stmt|drop table if exists "interviewer_metric" cascade|]
   ,[H.stmt|create table "interviewer_metric" (
        "metric_id"       bigint   not null
       ,"interviewer_id"  bigint   not null
       ,"score"           integer  not null
       ,constraint "one_score_per_interviewer_per_metric" unique ("metric_id", "interviewer_id")
    )|]
   ]

reviewer :: SchemaInit
reviewer = mapM_ H.unitEx [
    [H.stmt|drop table if exists "reviewer" cascade|]
   ,[H.stmt|create table "reviewer" (
        "id"              bigserial  primary key
       ,"member_id"       bigint     not null
       ,"application_id"  bigint     not null
       ,"review_start"    timestamp  not null
       ,"review_submit"   timestamp  not null
       ,constraint "one_review_per_member_per_application" unique ("member_id", "application_id")
    )|]
   ]

interviewer :: SchemaInit
interviewer = mapM_ H.unitEx [
    [H.stmt|drop table if exists "interviewer" cascade|]
   ,[H.stmt|create table "interviewer" (
        "id"              bigserial  primary key
       ,"member_id"       bigint     not null
       ,"application_id"  bigint     not null
       ,"interview_date"  timestamp  not null
       ,constraint "one_interview_per_member_per_application" unique ("member_id", "application_id")
    )|]
   ]

question :: SchemaInit
question = mapM_ H.unitEx [
    [H.stmt|drop table if exists "question" cascade|]
   ,[H.stmt|create table "question" (
        "id"      bigserial  primary key
       ,"active"  boolean    default true
       ,"query"   varchar    not null
    )|]
   ]

answer :: SchemaInit
answer = mapM_ H.unitEx [
    [H.stmt|drop table if exists "answer" cascade|]
   ,[H.stmt|create table "answer" (
        "application_id"  bigint   not null
       ,"question_id"     bigint   not null
       ,"response"        varchar  not null
       ,constraint "one_response_per_application_per_question" unique ("application_id", "question_id")
    )|]
   ]

housing_eval :: SchemaInit
housing_eval = mapM_ H.unitEx [
    [H.stmt|drop table if exists "housing_eval" cascade|]
   ,[H.stmt|create table "housing_eval" (
        "id"         bigserial  primary key
       ,"eval_date"  date       not null constraint "no_simultaneous_housing_evals" unique
    )|]
   ]

housing_evaluator :: SchemaInit
housing_evaluator = mapM_ H.unitEx [
    [H.stmt|drop table if exists "housing_evaluator" cascade|]
   ,[H.stmt|create table "housing_evaluator" (
        "housing_eval_id"  bigint   not null
       ,"member_id"        bigint   not null
       ,"score"            integer  not null
       ,"voted"            boolean  not null default false
       ,constraint "one_score_per_housing_eval" unique ("housing_eval_id", "member_id")
    )|]
   ]

term :: SchemaInit
term = mapM_ H.unitEx [
    [H.stmt|drop table if exists "term" cascade|]
   ,[H.stmt|create table "term" (
        "id"          bigint  primary key
       ,"start_date"  date    not null constraint "no_simultaneous_terms" unique
       ,"end_date"    date    default null
    )|]
   ]

dues :: SchemaInit
dues = mapM_ H.unitEx [
    [H.stmt|drop table if exists "dues" cascade|]
   ,[H.stmt|create table "dues" (
        "term_id"    bigint  not null
       ,"member_id"  bigint  not null
       ,"status"     dues_t  not null
       ,constraint "one_dues_status_per_term" unique ("term_id", "member_id")
    )|]
   ]

statement :: SchemaInit
statement = mapM_ H.unitEx [
    [H.stmt|drop table if exists "statement" cascade|]
   ,[H.stmt|create table "statement" (
        "id"            bigserial  primary key
       ,"sg_record"     varchar    not null constraint "unique_statement_group_record" unique
       ,"side_effects"  boolean    not null
    )|]
   ]

statement_exec :: SchemaInit
statement_exec = mapM_ H.unitEx [
    [H.stmt|drop table if exists "statement_exec" cascade|]
   ,[H.stmt|create table "statement_exec" (
        "statement_id"  bigint     not null
       ,"member_id"     bigint     not null
       ,"timestamp"     timestamp  not null
    )|]
   ]

enableForeignKeys :: SchemaInit
enableForeignKeys = mapM_ H.unitEx [
    [H.stmt|alter table "eboard" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "room" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "membership" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "event_attendee" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "event_attendee" add foreign key ("event_id") references "event" ("id")|]
   ,[H.stmt|alter table "project_participant" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "project_participant" add foreign key ("project_id") references "project" ("id")|]
   ,[H.stmt|alter table "evaluation" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "conditional" add foreign key ("evaluation_id") references "evaluation" ("id")|]
   ,[H.stmt|alter table "freshman_project_participant" add foreign key ("freshman_project_id") references "freshman_project" ("id")|]
   ,[H.stmt|alter table "freshman_project_participant" add foreign key ("evaluation_id") references "evaluation" ("id")|]
   ,[H.stmt|alter table "packet" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "signature" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "signature" add foreign key ("packet_id") references "packet" ("id")|]
   ,[H.stmt|alter table "queue" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "application" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "reviewer_metric" add foreign key ("metric_id") references "metric" ("id")|]
   ,[H.stmt|alter table "reviewer_metric" add foreign key ("reviewer_id") references "reviewer" ("id")|]
   ,[H.stmt|alter table "interviewer_metric" add foreign key ("metric_id") references "metric" ("id")|]
   ,[H.stmt|alter table "interviewer_metric" add foreign key ("interviewer_id") references "interviewer" ("id")|]
   ,[H.stmt|alter table "reviewer" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "reviewer" add foreign key ("application_id") references "application" ("id")|]
   ,[H.stmt|alter table "interviewer" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "interviewer" add foreign key ("application_id") references "application" ("id")|]
   ,[H.stmt|alter table "answer" add foreign key ("application_id") references "application" ("id")|]
   ,[H.stmt|alter table "answer" add foreign key ("question_id") references "question" ("id")|]
   ,[H.stmt|alter table "housing_evaluator" add foreign key ("housing_eval_id") references "housing_eval" ("id")|]
   ,[H.stmt|alter table "housing_evaluator" add foreign key ("member_id") references "member" ("id")|]
   ,[H.stmt|alter table "dues" add foreign key ("term_id") references "term" ("id")|]
   ,[H.stmt|alter table "statement_exec" add foreign key ("statement_id") references "statement" ("id")|]
   ,[H.stmt|alter table "statement_exec" add foreign key ("member_id") references "member" ("id")|]
   ]

enableIndices :: SchemaInit
enableIndices = mapM_ H.unitEx [
    [H.stmt|create index "member_id_index" on "member" ("id")|]
   ,[H.stmt|create index "member_uuid_index" on"member" ("uuid")|]
   ,[H.stmt|create index "member_username_index" on "member" ("username")|]
   ,[H.stmt|create index "member_commonname_index" on "member" ("commonname")|]
   ,[H.stmt|create index "member_onfloor_status_index" on "member" ("onfloor_status")|]
   ,[H.stmt|create index "eboard_member_id_index" on "eboard" ("member_id")|]
   ,[H.stmt|create index "room_member_id_index" on "room" ("member_id")|]
   ,[H.stmt|create index "room_room_number_index" on "room" ("room_number")|]
   ,[H.stmt|create index "membership_member_id_index" on "membership" ("member_id")|]
   ,[H.stmt|create index "membership_status_index" on "membership" ("status")|]
   ,[H.stmt|create index "event_id_index" on "event" ("id")|]
   ,[H.stmt|create index "event_title_index" on "event" ("title")|]
   ,[H.stmt|create index "event_category_index" on "event" ("category")|]
   ,[H.stmt|create index "event_attendee_member_id_index" on "event_attendee" ("member_id")|]
   ,[H.stmt|create index "event_attendee_event_id_index" on "event_attendee" ("event_id")|]
   ,[H.stmt|create index "project_id_index" on "project" ("id")|]
   ,[H.stmt|create index "project_title_index" on "project" ("title")|]
   ,[H.stmt|create index "project_status_index" on "project" ("status")|]
   ,[H.stmt|create index "project_participant_member_id_index" on "project_participant" ("member_id")|]
   ,[H.stmt|create index "project_participant_project_id_index" on "project_participant" ("project_id")|]
   ,[H.stmt|create index "evaluation_id_index" on "evaluation" ("id")|]
   ,[H.stmt|create index "evaluation_member_id_index" on "evaluation" ("member_id")|]
   ,[H.stmt|create index "evaluation_eval_type_index" on "evaluation" ("eval_type")|]
   ,[H.stmt|create index "evaluation_available_index" on "evaluation" ("available")|]
   ,[H.stmt|create index "conditional_id_index" on "conditional" ("id")|]
   ,[H.stmt|create index "conditional_evaluation_id" on "conditional" ("evaluation_id")|]
   ,[H.stmt|create index "freshman_project_id_index" on "freshman_project" ("id")|]
   ,[H.stmt|create index "freshman_project_participant_id_index" on "freshman_project_participant" ("freshman_project_id")|]
   ,[H.stmt|create index "freshman_project_participant_evaluation_id_index" on "freshman_project_participant" ("evaluation_id")|]
   ,[H.stmt|create index "packet_id_index" on "packet" ("id")|]
   ,[H.stmt|create index "packet_member_id_index" on "packet" ("member_id")|]
   ,[H.stmt|create index "signature_member_id_index" on "signature" ("member_id")|]
   ,[H.stmt|create index "signature_packet_id_index" on "signature" ("packet_id")|]
   ,[H.stmt|create index "queue_id_index" on "queue" ("id")|]
   ,[H.stmt|create index "queue_member_id_index" on "queue" ("member_id")|]
   ,[H.stmt|create index "application_id_index" on "application" ("id")|]
   ,[H.stmt|create index "application_member_id_index" on "application" ("member_id")|]
   ,[H.stmt|create index "metric_id_index" on "metric" ("id")|]
   ,[H.stmt|create index "reviewer_metric_metric_id_index" on "reviewer_metric" ("metric_id")|]
   ,[H.stmt|create index "reviewer_metric_reviewer_id_index" on "reviewer_metric" ("reviewer_id")|]
   ,[H.stmt|create index "interviewer_metric_metric_id_index" on "interviewer_metric" ("metric_id")|]
   ,[H.stmt|create index "interviewer_metric_interviewer_id_index" on "interviewer_metric" ("interviewer_id")|]
   ,[H.stmt|create index "reviewer_id_index" on "reviewer" ("id")|]
   ,[H.stmt|create index "reviewer_member_id_index" on "reviewer" ("member_id")|]
   ,[H.stmt|create index "reviewer_application_id_index" on "reviewer" ("application_id")|]
   ,[H.stmt|create index "interviewer_id_index" on "interviewer" ("id")|]
   ,[H.stmt|create index "interviewer_member_id_index" on "interviewer" ("member_id")|]
   ,[H.stmt|create index "interviewer_application_id_index" on "interviewer" ("application_id")|]
   ,[H.stmt|create index "question_id_index" on "question" ("id")|]
   ,[H.stmt|create index "answer_application_id_index" on "answer" ("application_id")|]
   ,[H.stmt|create index "answer_question_id_index" on "answer" ("question_id")|]
   ,[H.stmt|create index "housing_eval_id_index" on "housing_eval" ("id")|]
   ,[H.stmt|create index "housing_evaluator_housing_eval_id_index" on "housing_evaluator" ("housing_eval_id")|]
   ,[H.stmt|create index "housing_evaluator_member_id_index" on "housing_evaluator" ("member_id")|]
   ,[H.stmt|create index "term_id_index" on "term" ("id")|]
   ,[H.stmt|create index "dues_term_id_index" on "dues" ("term_id")|]
   ,[H.stmt|create index "dues_member_id_index" on "dues" ("member_id")|]
   ,[H.stmt|create index "statement_id_index" on "statement" ("id")|]
   ,[H.stmt|create index "statement_sg_record_index" on "statement" ("sg_record")|]
   ,[H.stmt|create index "statement_exec_statement_id_index" on "statement_exec" ("statement_id")|]
   ,[H.stmt|create index "statement_exec_member_id_index" on "statement_exec" ("member_id")|]
   ]